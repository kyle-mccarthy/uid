use std::{
    fmt,
    marker::PhantomData,
    str::{self, FromStr},
};

use fast32::base32::CROCKFORD_LOWER;
use serde::de::Visitor;
use uuid::Uuid;

/// A trait that controls the prefix and type name of an identifier.
pub trait Identifiable {
    /// The prefix that is used to identify this type of identifier.
    const PREFIX: &str;

    /// The name of the type that this identifier is associated with. Used for
    /// error messages.
    const TYPE_NAME: &'static str;
}

/// A universally unique identifier that is scoped to a specific entity type.
/// When serialized or stringified, the resulting identifier will be prefixed
/// with a "tag" that identifies the type of entity that the identifier is
/// associated with. The inner ID (the uuid) is encoded using Crockford's
/// base32.
#[derive(PartialEq, Eq, Hash)]
pub struct Uid<T: Identifiable> {
    value: Uuid,
    _marker: PhantomData<T>,
}

impl<T> Uid<T>
where
    T: Identifiable,
{
    /// Generate a new identifier with a random UUID. A version 7 UUID is used
    /// for the inner id type.
    pub fn new() -> Self {
        Self {
            value: Uuid::now_v7(),
            _marker: PhantomData,
        }
    }

    /// Create a new identifier with the given UUID.
    pub fn with_uuid(value: Uuid) -> Self {
        Self {
            value,
            _marker: PhantomData,
        }
    }

    /// Converts the typed identifier into a raw UUID value.
    pub fn into_uuid(self) -> Uuid {
        self.value
    }

    /// Creates a new typed identifier with a nil UUID.
    pub fn nil() -> Self {
        Self {
            value: Uuid::nil(),
            _marker: PhantomData,
        }
    }

    /// Parses a string representation of a typed identifier. This is an alias
    /// to the `FromStr` impl.
    pub fn parse(value: &str) -> Result<Uid<T>, ParseUidError<T>> {
        Uid::<T>::from_str(value)
    }
}

impl<T> Clone for Uid<T>
where
    T: Identifiable + Clone,
{
    fn clone(&self) -> Self {
        Self {
            value: self.value,
            _marker: PhantomData,
        }
    }
}

impl<T> Copy for Uid<T> where T: Identifiable + Clone + Copy {}

impl<T> Default for Uid<T>
where
    T: Identifiable,
{
    fn default() -> Self {
        Self {
            value: Uuid::now_v7(),
            _marker: PhantomData,
        }
    }
}

impl<T> std::ops::Deref for Uid<T>
where
    T: Identifiable,
{
    type Target = Uuid;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> fmt::Display for Uid<T>
where
    T: Identifiable,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let encoded = CROCKFORD_LOWER.encode_u128(self.value.as_u128());
        write!(f, "{}_{}", T::PREFIX, encoded)
    }
}

impl<T> fmt::Debug for Uid<T>
where
    T: Identifiable,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Uid")
            .field("id", &self.value)
            .field("prefix", &T::PREFIX)
            .finish()
    }
}

#[derive(Debug)]
pub struct ParseUidError<T: Identifiable> {
    kind: ErrorKind,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
    value: Box<str>,
    _marker: PhantomData<T>,
}

impl<T> ParseUidError<T>
where
    T: Identifiable,
{
    pub fn new(kind: ErrorKind, value: Box<str>) -> Self {
        Self {
            kind,
            source: None,
            value,
            _marker: PhantomData,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }

    pub fn parse(value: &str) -> Result<Uid<T>, ParseUidError<T>> {
        Uid::<T>::from_str(value)
    }

    /// The value that was attempted to be parsed.
    pub fn value(&self) -> &str {
        &self.value
    }

    /// The kind of error that occurred.
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }
}

impl<T> fmt::Display for ParseUidError<T>
where
    T: Identifiable,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse {} UID error.", T::TYPE_NAME)?;

        match self.kind {
            ErrorKind::Malformed => write!(f, " The value was not in the expected format.")?,
            ErrorKind::PrefixMismatch => write!(
                f,
                " The prefix did not match the expected value. Expected \"{}\".",
                T::PREFIX
            )?,
            ErrorKind::Decoder => write!(f, " The value could not be decoded.")?,
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Malformed,
    PrefixMismatch,
    Decoder,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::Malformed => write!(f, "the value was not in the expected format"),
            ErrorKind::PrefixMismatch => write!(f, "the pre"),
            ErrorKind::Decoder => write!(f, "Decoder"),
        }
    }
}

impl<T> str::FromStr for Uid<T>
where
    T: Identifiable,
{
    type Err = ParseUidError<T>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Make sure that the separator is a char boundary so we can safely split the
        // identifier into the prefix and the encoded value.
        let separator_index = T::PREFIX.len();

        if !s.is_char_boundary(separator_index) {
            return Err(ParseUidError::new(ErrorKind::Malformed, s.into()));
        }

        let (prefix, rest) = s.split_at(separator_index);

        if prefix != T::PREFIX {
            return Err(ParseUidError::new(ErrorKind::PrefixMismatch, s.into()));
        }

        // Make sure that it isn't empty or we will panic when we try to slice the
        // encoded contents.
        if rest.is_empty() {
            return Err(ParseUidError::new(ErrorKind::Malformed, s.into()));
        }

        let encoded = &rest[1..];
        let raw = CROCKFORD_LOWER.decode_u128_str(encoded).map_err(|source| {
            ParseUidError::new(ErrorKind::Decoder, s.into()).with_source(source)
        })?;

        Ok(Uid::with_uuid(Uuid::from_u128(raw)))
    }
}

impl<T> serde::ser::Serialize for Uid<T>
where
    T: Identifiable,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        let serialized = self.to_string();
        serializer.serialize_str(&serialized)
    }
}

impl<'de, T> serde::de::Deserialize<'de> for Uid<T>
where
    T: Identifiable,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct UidVisitor<T: Identifiable> {
            _marker: PhantomData<T>,
        }

        impl<'a, T> Visitor<'a> for UidVisitor<T>
        where
            T: Identifiable,
        {
            type Value = Uid<T>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("uid")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Uid::from_str(v).map_err(|err| serde::de::Error::custom(err))
            }
        }

        Ok(deserializer.deserialize_str(UidVisitor {
            _marker: PhantomData,
        })?)
    }
}

#[cfg(test)]
mod test_uid {
    use std::str::FromStr;

    use uuid::Uuid;

    use crate::{Identifiable, Uid};

    #[derive(Debug, PartialEq)]
    struct User;

    impl Identifiable for User {
        const PREFIX: &'static str = "usr";

        const TYPE_NAME: &'static str = "User";
    }

    struct Account;

    impl Identifiable for Account {
        const PREFIX: &'static str = "acct";

        const TYPE_NAME: &'static str = "Account";
    }

    #[test]
    fn it_roundtrips() {
        let uid = Uid::<User>::default();

        let string_repr = uid.to_string();

        let parsed_uid = Uid::<User>::from_str(&string_repr).unwrap();
        assert_eq!(uid, parsed_uid);
    }

    #[test]
    fn it_is_equal() {
        let raw = Uuid::now_v7();

        let uid1 = Uid::<User>::with_uuid(raw);
        let uid2 = Uid::<User>::with_uuid(raw);

        assert_eq!(uid1, uid2);
    }

    #[test]
    fn it_does_enforces_prefix_match_on_decode() {
        let inner = Uuid::now_v7();

        let user_id = Uid::<User>::with_uuid(inner).to_string();

        let account_id = Uid::<Account>::parse(&user_id).unwrap_err();
        assert_eq!(account_id.kind(), crate::ErrorKind::PrefixMismatch);
    }
}

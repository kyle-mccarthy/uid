# uid

A Rust library for type-safe, prefixed UUIDs that are encoded using Crockford's base32.

## Features

- Type-safe UUID wrapper with customizable prefixes
- Crockford's base32 encoding for compact string representation
- Serde support for serialization/deserialization
- Built-in error handling for parsing
- Clear error messages
- Uses UUID v7 by default (time-based ordering)

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
uid = "0.1.0"
```

### Basic Example

```rust
use uid::{Identifiable, Uid};

// Define your entity type
struct Account;

impl Identifiable for Account {
    const PREFIX: &'static str = "acct";
    const TYPE_NAME: &'static str = "account";
}

fn main() {
    // Create a new UUID
    let account_id = Uid::<Account>::new();

    // Convert to string (includes prefix)
    let string_repr = account_id.to_string(); // e.g. "acct_1h9r4j5k8n..."

    // Parse from string
    let parsed_id = Uid::<Account>::parse(&string_repr).unwrap();

    assert_eq!(account_id, parsed_id);
}
```

### Working with Different Types

The type system ensures that IDs for different entity types can't be mixed up:

```rust
struct User;

impl Identifiable for User {
    const PREFIX: &'static str = "usr";
    const TYPE_NAME: &'static str = "User";
}

// These are different types and can't be used interchangeably
let user_id: Uid<User> = Uid::new();
let account_id: Uid<Account> = Uid::new();
```

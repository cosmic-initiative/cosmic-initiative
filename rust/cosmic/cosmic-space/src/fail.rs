use alloc::string::String;

use serde::{Deserialize, Serialize};

use crate::err::SpaceErr;

pub mod mesh {
    use alloc::string::String;

    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Fail {
        Error(String),
    }
}

pub mod portal {
    use alloc::string::String;

    use serde::{Deserialize, Serialize};

    use crate::fail::{ext, http, resource};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Fail {
        Error(String),
        Resource(resource::Fail),
        Ext(ext::Fail),
        Http(http::Error),
    }
}

pub mod http {
    use alloc::string::String;

    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Error {
        pub message: String,
    }
}

pub mod resource {
    use alloc::string::String;

    use serde::{Deserialize, Serialize};

    use crate::fail::{Bad, BadCoercion, BadRequest, Conditional, Messaging, NotFound};
    use crate::loc::Point;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Fail {
        Create(Create),
        Update(Update),
        Select(Select),
        BadRequest(BadRequest),
        Conditional(Conditional),
        Messaging(Messaging),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Create {
        AddressAlreadyInUse(String),
        WrongParentResourceType { expected: String, found: String },
        CannotUpdateArchetype,
        InvalidProperty { expected: String, found: String },
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Update {
        Immutable,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Select {
        WrongAddress { required: Point, found: Point },
        BadSelectRouting { required: String, found: String },
        BadCoercion(BadCoercion),
    }
}

pub mod ext {
    use alloc::string::String;

    use serde::{Deserialize, Serialize};

    use crate::fail::{BadRequest, Conditional};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Fail {
        Error(String),
        BadRequest(BadRequest),
        Conditional(Conditional),
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BadRequest {
    NotFound(NotFound),
    Bad(Bad),
    Illegal(Illegal),
    Wrong(Wrong),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BadCoercion {
    pub from: String,
    pub into: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Conditional {
    Timeout(Timeout),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Timeout {
    pub waited: i32,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NotFound {
    Any,
    ResourceType(String),
    Kind(String),
    Specific(String),
    Address(String),
    Key(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Bad {
    ResourceType(String),
    Kind(String),
    Specific(String),
    Address(String),
    Key(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Identifier {
    ResourceType,
    Kind,
    Specific,
    Address,
    Key,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Illegal {
    Immutable,
    EmptyToFieldOnMessage,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Wrong {
    pub received: String,
    pub expected: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Messaging {
    RequestReplyExchangesRequireOneAndOnlyOneRecipient,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Fail {
    Mesh(mesh::Fail),
    Resource(resource::Fail),
    Portal(portal::Fail),
    Error(String),
}

impl ToString for Fail {
    fn to_string(&self) -> String {
        "Fail".to_string()
    }
}

/*    impl Into<ExtErr> for Fail {
       fn into(self) -> ExtErr {
           ExtErr {
               status: 500,
               message: "Fail".to_string(),
           }
       }
   }

*/

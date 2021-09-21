use crate::resource::{ResourceKind, ResourceAddress,ArtifactKind};
use crate::artifact::ArtifactRef;
use crate::cache::{Cacheable, Data};
use crate::resource::config::{ResourceConfig, Parser};
use crate::config::mechtron::yaml::MechtronConfigYaml;
use std::sync::Arc;
use crate::error::Error;
use std::str::FromStr;
use std::convert::TryInto;
use starlane_resources::ResourcePath;

pub struct MechtronConfig {
    pub name: String,
    pub prefix: String,
    pub artifact: ResourcePath,
    pub bind: ArtifactRef,
    pub wasm: ArtifactRef,
}

impl Cacheable for MechtronConfig {
    fn artifact(&self) -> ArtifactRef {
        ArtifactRef {
            path: self.artifact.clone(),
            kind: ArtifactKind::MechtronConfig,
        }
    }

    fn references(&self) -> Vec<ArtifactRef> {
        vec![self.bind.clone(),self.wasm.clone()]
    }
}

impl ResourceConfig for MechtronConfigParser {
    fn kind(&self) -> ResourceKind {
        ResourceKind::Mechtron
    }
}

pub struct MechtronConfigParser;

impl MechtronConfigParser {
    pub fn new() -> Self {
        Self {}
    }
}

impl Parser<MechtronConfig> for MechtronConfigParser {
    fn parse(&self, artifact: ArtifactRef, _data: Data) -> Result<Arc<MechtronConfig>, Error> {

        let data = String::from_utf8((*_data).clone() )?;
        let yaml: MechtronConfigYaml = serde_yaml::from_str( data.as_str() )?;

        let address = artifact.path.clone();
        let bundle_address = address.parent().ok_or::<Error>("expected artifact to have bundle parent".into())?;

        let bind = yaml.spec.bind.replace("{bundle}", bundle_address.to_string().as_str() );
        let bind= ResourcePath::from_str(bind.as_str() )?;
        let bind = ArtifactRef::new(bind.try_into()?,ArtifactKind::BindConfig);

        let wasm = yaml.spec.wasm.replace("{bundle}", bundle_address.to_string().as_str() );
        let wasm= ResourcePath::from_str(wasm.as_str() )?;
        let wasm = ArtifactRef::new(wasm.try_into()?,ArtifactKind::Wasm);

        Ok(Arc::new(MechtronConfig {
            artifact: artifact.path,
            bind,
            wasm,
            name: yaml.spec.name,
            prefix: yaml.spec.prefix
        }))
    }
}

mod yaml {
    use serde::{Serialize,Deserialize};

    #[derive(Clone, Serialize, Deserialize)]
    pub struct MechtronConfigYaml {
        pub kind: String,
        pub spec: SpecYaml
    }

    #[derive(Clone, Serialize, Deserialize)]
    pub struct SpecYaml {
        pub name: String,
        pub prefix: String,
        pub bind: String,
        pub wasm: String
    }
}


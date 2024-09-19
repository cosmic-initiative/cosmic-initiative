use crate::host::{err, ExtProcess, Proc};
use crate::host::{Executor, ExeEnv, ExtKey, ExeService, StdinProc};
use std::collections::HashMap;
use std::process::Stdio;
use std::sync::Arc;
use tokio::io::Stdin;
use tokio::process::{Child, ChildStdin, Command};
use crate::host::err::HostErr;

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct ExtBin {
    file: String,
}

impl ToString for ExtBin {
    fn to_string(&self) -> String {
        self.file.clone()
    }
}

impl ExtBin {
    pub fn new(file: String) -> Self {
        Self { file }
    }
}

pub struct ExtHostService {
    hosts: HashMap<ExtKey<ExtBin>, Arc<Ext>>,
}

impl ExtHostService {
    pub fn new() -> Self {
        Self {
            hosts: Default::default(),
        }
    }
}

#[async_trait]
impl ExeService<ExtBin, Child, tokio::io::Stdin> for ExtHostService {
    async fn provision(
        &mut self,
        bin: ExtBin,
        env: ExeEnv,
    ) -> Result<Box<dyn Executor<Child, tokio::io::Stdin>>, crate::host::err::HostErr> {
        let key = ExtKey::new(bin.clone(), env.clone());
        return Ok(Box::new(Ext::new(bin.clone(), env)));
    }
}


pub enum Ext {
    Wasm,
    Ext
}

impl Ext {
    fn new(bin: ExtBin, env: ExeEnv) -> Self {
        Self { env, bin }
    }

    async fn pre_exec(&self, args: Vec<String>) -> Result<Command, err::HostErr> {
        let mut command = Command::new(self.bin.file.clone());
        command.envs(self.env.env.clone());
        command.args(args.clone());
        command.current_dir(self.env.pwd.clone());
        command.env_clear();
        command.stdout(Stdio::piped());
        command.stderr(Stdio::piped());
        Ok(command)
    }
}

#[async_trait]
impl Executor for Ext {
    type Err = HostErr;
    type Proc = ExtProcess;

    async fn execute(&self, args: Vec<String>) -> Result<Self::Proc, Self::Err> {
        let mut command = self.pre_exec(args).await?;
        command.stdin(Stdio::null());
        Ok(ExtProcess::new(command.spawn()?))
    }
}

pub struct ExtStdinProc {
    stdin: ChildStdin,
    child: Child,
}

impl ExtStdinProc {
    pub fn new(child: Child, stdin: ChildStdin) -> Self {
        Self { child, stdin }
    }
}

#[cfg(test)]
pub mod test {
    use crate::host::err::HostErr;
    use crate::host::ext::{ExtBin, ExtHostService};
    use crate::host::{ExtEnvBuilder, ExeService};
    use std::env::current_dir;

    #[tokio::test]
    pub async fn test() -> Result<(), HostErr> {
        let mut service = ExtHostService::new();
        let mut builder = ExtEnvBuilder::default();
        builder.pwd(format!("{}/bins", current_dir().unwrap().to_str().unwrap()));
        let bin = ExtBin::new("./filestore".to_string());
        let mut host = service.provision(bin, builder.build()).await.unwrap();

        let child = host.execute(vec!["list".to_string()]).await?;

        let output = child.wait_with_output().await?;

        let out = String::from_utf8(output.stdout).unwrap();
        println!("{}", out);
        Ok(())
    }
}

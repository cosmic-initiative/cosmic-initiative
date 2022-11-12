use std::ops::{Deref, DerefMut};

use crate::point::Layer;
use crate::log::{SpanLogger, Trackable};
use crate::wave::exchange::asynch::Exchanger;
use crate::wave::{DirectedWave, Ping, Pong, ReflectedWave, SingularDirectedWave, UltraWave, Wave};
use crate::{ParticleRecord, Point, SpaceErr, Surface};

#[async_trait]
pub trait TraversalLayer {
    fn surface(&self) -> Surface;

    async fn traverse_next(&self, traversal: Traversal<UltraWave>);
    async fn inject(&self, wave: UltraWave);

    fn exchanger(&self) -> &Exchanger;

    async fn deliver_directed(&self, direct: Traversal<DirectedWave>) -> Result<(), SpaceErr> {
        Err(SpaceErr::server_error(
            "this layer does not handle directed messages",
        ))
    }

    async fn deliver_reflected(&self, reflect: Traversal<ReflectedWave>) -> Result<(), SpaceErr> {
        self.exchanger().reflected(reflect.payload).await
    }

    async fn visit(&self, traversal: Traversal<UltraWave>) -> Result<(), SpaceErr> {
        if let Some(dest) = &traversal.dest {
            if self.surface().layer == *dest {
                if traversal.is_directed() {
                    self.deliver_directed(traversal.unwrap_directed()).await?;
                } else {
                    self.deliver_reflected(traversal.unwrap_reflected()).await?;
                }
                return Ok(());
            } else {
            }
        }

        if traversal.is_directed() && traversal.dir == TraversalDirection::Fabric {
            self.directed_fabric_bound(traversal.unwrap_directed())
                .await?;
        } else if traversal.is_reflected() && traversal.dir == TraversalDirection::Core {
            self.reflected_core_bound(traversal.unwrap_reflected())
                .await?;
        } else if traversal.is_directed() && traversal.dir == TraversalDirection::Core {
            self.directed_core_bound(traversal.unwrap_directed())
                .await?;
        } else if traversal.is_reflected() && traversal.dir == TraversalDirection::Fabric {
            self.reflected_fabric_bound(traversal.unwrap_reflected())
                .await?;
        }

        Ok(())
    }

    // override if you want to track outgoing requests
    async fn directed_fabric_bound(
        &self,
        mut traversal: Traversal<DirectedWave>,
    ) -> Result<(), SpaceErr> {
        self.traverse_next(traversal.wrap()).await;
        Ok(())
    }

    async fn directed_core_bound(
        &self,
        mut traversal: Traversal<DirectedWave>,
    ) -> Result<(), SpaceErr> {
        self.traverse_next(traversal.wrap()).await;
        Ok(())
    }

    // override if you want to track incoming responses
    async fn reflected_core_bound(
        &self,
        traversal: Traversal<ReflectedWave>,
    ) -> Result<(), SpaceErr> {
        self.traverse_next(traversal.to_ultra()).await;
        Ok(())
    }

    async fn reflected_fabric_bound(
        &self,
        traversal: Traversal<ReflectedWave>,
    ) -> Result<(), SpaceErr> {
        self.traverse_next(traversal.to_ultra()).await;
        Ok(())
    }
}

#[derive(Clone)]
pub struct TraversalPlan {
    pub stack: Vec<Layer>,
}

impl TraversalPlan {
    pub fn new(stack: Vec<Layer>) -> Self {
        Self { stack }
    }

    pub fn towards_fabric(&self, layer: &Layer) -> Option<Layer> {
        let mut layer = layer.clone();
        let mut index: i32 = layer.ordinal() as i32;
        loop {
            index = index - 1;

            if index < 0i32 {
                return None;
            } else if self
                .stack
                .contains(&Layer::from_ordinal(index as u8).unwrap())
            {
                return Some(Layer::from_ordinal(index as u8).unwrap());
            }
        }
    }

    pub fn towards_core(&self, layer: &Layer) -> Option<Layer> {
        let mut layer = layer.clone();
        let mut index = layer.ordinal();
        loop {
            index = index + 1;
            let layer = match Layer::from_ordinal(index) {
                Some(layer) => layer,
                None => {
                    return None;
                }
            };

            if self.stack.contains(&layer) {
                return Some(layer);
            }
        }
    }

    pub fn has_layer(&self, layer: &Layer) -> bool {
        self.stack.contains(layer)
    }
}

#[derive(Clone)]
pub struct TraversalInjection {
    pub surface: Surface,
    pub wave: UltraWave,
    pub from_gravity: bool,
    pub dir: Option<TraversalDirection>,
}

impl TraversalInjection {
    pub fn new(injector: Surface, wave: UltraWave) -> Self {
        Self {
            surface: injector,
            wave,
            from_gravity: false,
            dir: None,
        }
    }
}

#[derive(Clone)]
pub struct Traversal<W> {
    pub point: Point,
    pub payload: W,
    pub record: ParticleRecord,
    pub layer: Layer,
    pub dest: Option<Layer>,
    pub logger: SpanLogger,
    pub dir: TraversalDirection,
    pub to: Surface,
}

impl<W> Trackable for Traversal<W>
where
    W: Trackable,
{
    fn track_id(&self) -> String {
        self.payload.track_id()
    }

    fn track_method(&self) -> String {
        self.payload.track_method()
    }

    fn track_payload(&self) -> String {
        self.payload.track_payload()
    }

    fn track_from(&self) -> String {
        self.payload.track_from()
    }

    fn track_to(&self) -> String {
        self.payload.track_to()
    }

    fn track(&self) -> bool {
        self.payload.track()
    }
}

#[derive(Clone, Eq, PartialEq, Hash, strum_macros::Display)]
pub enum TraversalDirection {
    Fabric,
    Core,
}

impl TraversalDirection {
    pub fn new(from: &Layer, to: &Layer) -> Result<Self, SpaceErr> {
        if from == to {
            return Err(
                "cannot determine traversal direction if from and to are the same layer".into(),
            );
        } else if from.ordinal() < to.ordinal() {
            Ok(TraversalDirection::Core)
        } else {
            Ok(TraversalDirection::Fabric)
        }
    }

    pub fn is_fabric(&self) -> bool {
        match self {
            TraversalDirection::Fabric => true,
            TraversalDirection::Core => false,
        }
    }
    pub fn is_core(&self) -> bool {
        match self {
            TraversalDirection::Fabric => false,
            TraversalDirection::Core => true,
        }
    }
}

impl TraversalDirection {
    pub fn reverse(&self) -> TraversalDirection {
        match self {
            Self::Fabric => Self::Core,
            Self::Core => Self::Fabric,
        }
    }
}

impl<W> Traversal<W> {
    pub fn new(
        payload: W,
        record: ParticleRecord,
        layer: Layer,
        logger: SpanLogger,
        dir: TraversalDirection,
        dest: Option<Layer>,
        to: Surface,
        point: Point,
    ) -> Self {
        Self {
            payload,
            record,
            layer,
            logger,
            dir,
            dest,
            to,
            point,
        }
    }

    pub fn extract(self) -> W {
        self.payload
    }

    pub fn with<N>(self, payload: N) -> Traversal<N> {
        Traversal {
            payload,
            record: self.record,
            layer: self.layer,
            logger: self.logger,
            dir: self.dir,
            dest: self.dest,
            to: self.to,
            point: self.point,
        }
    }

    pub fn reverse(&mut self) {
        self.dir = self.dir.reverse();
    }
}

impl<W> Traversal<W> {
    pub fn next(&mut self) -> Option<Layer> {
        let next = match self.dir {
            TraversalDirection::Fabric => self
                .record
                .details
                .stub
                .kind
                .wave_traversal_plan()
                .towards_fabric(&self.layer),
            TraversalDirection::Core => self
                .record
                .details
                .stub
                .kind
                .wave_traversal_plan()
                .towards_core(&self.layer),
        };
        match &next {
            None => {}
            Some(layer) => {
                self.layer = layer.clone();
            }
        }
        next
    }

    pub fn is_inter_layer(&self) -> bool {
        self.to.point == *self.logger.point()
    }
}

impl Traversal<UltraWave> {
    pub fn is_fabric_bound(&self) -> bool {
        match self.dir {
            TraversalDirection::Fabric => true,
            TraversalDirection::Core => false,
        }
    }

    pub fn is_core_bound(&self) -> bool {
        match self.dir {
            TraversalDirection::Fabric => false,
            TraversalDirection::Core => true,
        }
    }

    pub fn is_ping(&self) -> bool {
        match &self.payload {
            UltraWave::Ping(_) => true,
            _ => false,
        }
    }

    pub fn is_pong(&self) -> bool {
        match &self.payload {
            UltraWave::Pong(_) => true,
            _ => false,
        }
    }

    pub fn is_directed(&self) -> bool {
        match self.payload {
            UltraWave::Ping(_) => true,
            UltraWave::Pong(_) => false,
            UltraWave::Ripple(_) => true,
            UltraWave::Echo(_) => false,
            UltraWave::Signal(_) => true,
        }
    }

    pub fn is_reflected(&self) -> bool {
        !self.is_directed()
    }

    pub fn unwrap_directed(self) -> Traversal<DirectedWave> {
        let clone = self.clone();
        match self.payload {
            UltraWave::Ping(ping) => clone.with(ping.to_directed().clone()),
            UltraWave::Ripple(ripple) => clone.with(ripple.to_directed()),
            UltraWave::Signal(signal) => clone.with(signal.to_directed()),
            _ => {
                panic!("cannot call this unless you are sure it's a DirectedWave")
            }
        }
    }

    pub fn unwrap_singular_directed(self) -> Traversal<SingularDirectedWave> {
        let clone = self.clone();
        match self.payload {
            UltraWave::Ping(ping) => clone.with(ping.to_singular_directed()),
            UltraWave::Ripple(ripple) => {
                clone.with(ripple.to_singular_directed().expect("singular directed"))
            }
            UltraWave::Signal(signal) => clone.with(signal.to_singular_directed()),
            _ => {
                panic!("cannot call this unless you are sure it's a DirectedWave")
            }
        }
    }

    pub fn unwrap_reflected(self) -> Traversal<ReflectedWave> {
        let clone = self.clone();
        match self.payload {
            UltraWave::Pong(pong) => clone.with(pong.to_reflected()),
            UltraWave::Echo(echo) => clone.with(echo.to_reflected()),
            _ => {
                panic!("cannot call this unless you are sure it's a ReflectedWave")
            }
        }
    }

    pub fn unwrap_ping(self) -> Traversal<Wave<Ping>> {
        if let UltraWave::Ping(ping) = self.payload.clone() {
            self.with(ping)
        } else {
            panic!("cannot call this unless you are sure it's a Ping")
        }
    }

    pub fn unwrap_pong(self) -> Traversal<Wave<Pong>> {
        if let UltraWave::Pong(pong) = self.payload.clone() {
            self.with(pong)
        } else {
            panic!("cannot call this unless you are sure it's a Pong")
        }
    }
}

impl Traversal<DirectedWave> {
    pub fn wrap(self) -> Traversal<UltraWave> {
        let ping = self.payload.clone();
        self.with(ping.to_ultra())
    }
}

impl Traversal<ReflectedWave> {
    pub fn wrap(self) -> Traversal<UltraWave> {
        let ping = self.payload.clone();
        self.with(ping.to_ultra())
    }
}

impl Traversal<SingularDirectedWave> {
    pub fn wrap(self) -> Traversal<UltraWave> {
        let ping = self.payload.clone();
        self.with(ping.to_ultra())
    }
}

impl Traversal<ReflectedWave> {
    pub fn to_ultra(self) -> Traversal<UltraWave> {
        let pong = self.payload.clone();
        self.with(pong.to_ultra())
    }
}

impl Traversal<Wave<Ping>> {
    pub fn to_ultra(self) -> Traversal<UltraWave> {
        let ping = self.payload.clone();
        self.with(ping.to_ultra())
    }

    pub fn to_directed(self) -> Traversal<DirectedWave> {
        let ping = self.payload.clone();
        self.with(ping.to_directed())
    }
}

impl Traversal<Wave<Pong>> {
    pub fn to_ultra(self) -> Traversal<UltraWave> {
        let pong = self.payload.clone();
        self.with(pong.to_ultra())
    }

    pub fn to_reflected(self) -> Traversal<ReflectedWave> {
        let pong = self.payload.clone();
        self.with(pong.to_reflected())
    }
}

impl<W> Deref for Traversal<W> {
    type Target = W;

    fn deref(&self) -> &Self::Target {
        &self.payload
    }
}

impl<W> DerefMut for Traversal<W> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.payload
    }
}

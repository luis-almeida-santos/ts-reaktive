package com.tradeshift.reaktive.testkit;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import static com.tradeshift.reaktive.testkit.Await.*;

import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;
import akka.persistence.AbstractPersistentActor;
import akka.testkit.TestProbe;
import scala.PartialFunction;
import scala.concurrent.duration.FiniteDuration;
import scala.runtime.BoxedUnit;

public class AkkaPersistence {
    private final ActorSystem system;
    
    public static void awaitPersistenceInit(ActorSystem system) {
        new AkkaPersistence(system).awaitPersistenceInit();
    }

    public AkkaPersistence(ActorSystem system) {
        this.system = system;
    }
    
    public void awaitPersistenceInit() {
        AtomicInteger n = new AtomicInteger();
        TestProbe probe = TestProbe.apply(system);
        within(45, TimeUnit.SECONDS).eventuallyDo(() -> {
            system.actorOf(Props.create(AwaitPersistenceInit.class), "persistenceInit" + n.incrementAndGet()).tell("hello", probe.ref());
            probe.expectMsg(FiniteDuration.create(5, TimeUnit.SECONDS), "hello");            
        });
    }
    
    private static class AwaitPersistenceInit extends AbstractPersistentActor {
        @Override
        public PartialFunction<Object, BoxedUnit> receiveCommand() {
            return ReceiveBuilder.matchAny(msg -> {
                persist(msg, m -> {
                    sender().tell(msg, self());
                    context().stop(self());
                });
            }).build();
        }

        @Override
        public PartialFunction<Object, BoxedUnit> receiveRecover() {
            return ReceiveBuilder.matchAny(msg -> {}).build();
        }

        @Override
        public String persistenceId() {
            return "ts-reaktive-testkit-awaitPersistenceInit";
        }
    }
    
}

package cn.blmdz.home.zookeeper.pubsub;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.api.ACLBackgroundPathAndBytesable;
import org.apache.curator.framework.api.BackgroundPathable;
import org.apache.curator.framework.api.CuratorWatcher;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.WatchedEvent;

import cn.blmdz.home.zookeeper.ZKClientFactory;

public class Subscriber {
   private final CuratorFramework client;
   private final Paths paths;

   public Subscriber(ZKClientFactory zkClientFactory, String topic) throws Exception {
      this(zkClientFactory, "/pubsub", topic);
   }

   public Subscriber(ZKClientFactory zkClientFactory, String basePath, String topic) throws Exception {
      this.client = zkClientFactory.getClient();
      this.paths = new Paths(basePath, topic);
      if(this.client.checkExists().forPath(this.paths.getClientBase()) == null) {
         this.client.create().creatingParentsIfNeeded().forPath(this.paths.getClientBase());
      }

      if(this.client.checkExists().forPath(this.paths.getTopicBase()) == null) {
         this.client.create().creatingParentsIfNeeded().forPath(this.paths.getTopicBase());
      }

      if(this.client.checkExists().forPath(this.paths.getSubscriberBase()) == null) {
         this.client.create().creatingParentsIfNeeded().forPath(this.paths.getSubscriberBase());
      }

      if(this.client.checkExists().forPath(this.paths.getClientPathOfLocalhost()) == null) {
         ((ACLBackgroundPathAndBytesable)this.client.create().withMode(CreateMode.EPHEMERAL)).forPath(this.paths.getClientPathOfLocalhost());
      }

      if(this.client.checkExists().forPath(this.paths.getSubscriberPathOfLocalhost()) == null) {
         ((ACLBackgroundPathAndBytesable)this.client.create().withMode(CreateMode.EPHEMERAL)).forPath(this.paths.getSubscriberPathOfLocalhost());
      }

      if(this.client.checkExists().forPath(this.paths.getTopicPathOfLocalHost()) == null) {
         this.client.create().forPath(this.paths.getTopicPathOfLocalHost());
      }

   }

   public void subscribe(final SubscribeCallback callback) throws Exception {
      CuratorWatcher callbackWatcher = new CuratorWatcher() {
         public void process(WatchedEvent event) throws Exception {
            byte[] data = (byte[])((BackgroundPathable)Subscriber.this.client.getData().usingWatcher(this)).forPath(Subscriber.this.paths.getTopicPathOfLocalHost());
            callback.fire(data);
         }
      };
      ((BackgroundPathable)this.client.getData().usingWatcher(callbackWatcher)).forPath(this.paths.getTopicPathOfLocalHost());
   }
}

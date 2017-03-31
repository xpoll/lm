package cn.blmdz.home.zookeeper.leader;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.recipes.leader.LeaderLatch;
import org.apache.curator.framework.recipes.leader.LeaderLatchListener;

import cn.blmdz.home.zookeeper.ZKClientFactory;

public class LeadLatchInitiator {
   private final CuratorFramework client;
   private final String path;
   private LeaderLatch leaderLatch;

   public LeadLatchInitiator(ZKClientFactory zkClientFactory, String path) throws Exception {
      this.client = zkClientFactory.getClient();
      this.path = path;
   }

   public void init(String identity, LeaderLatchListener... listeners) throws Exception {
      this.leaderLatch = new LeaderLatch(this.client, this.path, identity);

      for(LeaderLatchListener listener : listeners) {
         this.leaderLatch.addListener(listener);
      }

      this.leaderLatch.start();
   }

   public boolean isLeader() throws Exception {
      return this.leaderLatch.hasLeadership();
   }

   public String currentLeaderId() throws Exception {
      return this.leaderLatch.getLeader().getId();
   }

   public void close() throws Exception {
      this.leaderLatch.close();
   }
}

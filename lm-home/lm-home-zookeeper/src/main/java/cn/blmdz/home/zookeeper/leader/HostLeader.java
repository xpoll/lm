package cn.blmdz.home.zookeeper.leader;

import org.apache.curator.framework.recipes.leader.LeaderLatchListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.home.zookeeper.HostNames;

public class HostLeader {
   private static final Logger log = LoggerFactory.getLogger(HostLeader.class);
   private final LeadLatchInitiator leadLatchInitiator;

   public HostLeader() {
      this((LeadLatchInitiator)null);
   }

   public HostLeader(LeadLatchInitiator leadLatchInitiator) {
      this.leadLatchInitiator = leadLatchInitiator;
   }

   public void init() throws Exception {
      if(this.leadLatchInitiator != null) {
         this.leadLatchInitiator.init(HostNames.hostName, new LeaderLatchListener[0]);
      }

   }

   public String currentLeaderId() {
      try {
         return this.leadLatchInitiator != null?this.leadLatchInitiator.currentLeaderId():HostNames.hostName;
      } catch (Exception var2) {
         log.error("failed to get current leader id", var2);
         return "unknown";
      }
   }

   public boolean isLeader() {
      if(this.leadLatchInitiator == null) {
         return true;
      } else {
         try {
            return this.leadLatchInitiator.isLeader();
         } catch (Exception var2) {
            log.error("oops, zookeeper failed,", var2);
            return false;
         }
      }
   }

   public void destroy() throws Exception {
      if(this.leadLatchInitiator != null) {
         this.leadLatchInitiator.close();
      }

   }
}

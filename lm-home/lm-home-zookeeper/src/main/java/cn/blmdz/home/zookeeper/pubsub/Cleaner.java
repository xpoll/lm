package cn.blmdz.home.zookeeper.pubsub;

import java.util.List;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.PathChildrenCache;
import org.apache.curator.framework.recipes.cache.PathChildrenCache.StartMode;
import org.apache.curator.utils.ZKPaths;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Function;
import com.google.common.collect.Lists;

import cn.blmdz.home.zookeeper.ZKClientFactory;

public class Cleaner {
   private static final Logger log = LoggerFactory.getLogger(Cleaner.class);
   private final PathChildrenCache liveHostsPcc;
   private final PathChildrenCache topicPcc;
   private final Paths paths;
   private final CuratorFramework client;

   public Cleaner(ZKClientFactory zkClientFactory, String topic) throws Exception {
      this(zkClientFactory, "/pubsub", topic);
   }

   public Cleaner(ZKClientFactory zkClientFactory, String basePath, String topic) throws Exception {
      this.client = zkClientFactory.getClient();
      this.paths = new Paths(basePath, topic);
      this.liveHostsPcc = new PathChildrenCache(this.client, this.paths.getClientBase(), false);
      this.liveHostsPcc.start(StartMode.BUILD_INITIAL_CACHE);
      this.topicPcc = new PathChildrenCache(this.client, this.paths.getTopicBase(), false);
      this.topicPcc.start(StartMode.BUILD_INITIAL_CACHE);
   }

   public void clean() throws Exception {
      List<ChildData> liveHostsChildData = this.liveHostsPcc.getCurrentData();
      if(liveHostsChildData.isEmpty()) {
         log.info("no client exists for path: {}", this.paths.getClientBase());
      } else {
         List<String> liveHosts = Lists.transform(liveHostsChildData, new Function<ChildData, String>() {
            public String apply(ChildData child) {
               return ZKPaths.getNodeFromPath(child.getPath());
            }
         });
         List<ChildData> topicChildData = this.topicPcc.getCurrentData();
         if(topicChildData.isEmpty()) {
            log.info("no topic exists for path: {}", this.paths.getTopicBase());
         } else {
            for(ChildData child : topicChildData) {
               String hostName = ZKPaths.getNodeFromPath(child.getPath());
               if(!liveHosts.contains(hostName)) {
                  this.client.delete().forPath(this.paths.getTopicPathOfHost(hostName));
               }
            }

         }
      }
   }
}

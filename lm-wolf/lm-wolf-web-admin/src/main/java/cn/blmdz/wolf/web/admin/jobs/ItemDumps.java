package cn.blmdz.wolf.web.admin.jobs;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.zookeeper.leader.HostLeader;
import cn.blmdz.wolf.parana.search.item.ItemDumpService;

@RestController
@RequestMapping({"/api/search/item/"})
public class ItemDumps {
   private static final Logger log = LoggerFactory.getLogger(ItemDumps.class);
   private final ItemDumpService itemDumpService;
   private final HostLeader hostLeader;

   @Autowired
   public ItemDumps(ItemDumpService itemDumpService, HostLeader hostLeader) {
      this.itemDumpService = itemDumpService;
      this.hostLeader = hostLeader;
   }

   @RequestMapping(
      value = {"full"},
      produces = {"application/json"}
   )
   @Scheduled(
      cron = "0 0 1 * * ?"
   )
   public void fullDump() {
      if(!this.hostLeader.isLeader()) {
         log.info("current leader is:{}, skip", this.hostLeader.currentLeaderId());
      } else {
         log.info("full dump fired");
         this.itemDumpService.fullDump();
         log.info("full dump end");
      }
   }

   @Scheduled(
      cron = "0 */15 * * * ?"
   )
   @RequestMapping(
      value = {"delta"},
      produces = {"application/json"}
   )
   public void deltaDump() {
      if(!this.hostLeader.isLeader()) {
         log.info("current leader is:{}, skip ", this.hostLeader.currentLeaderId());
      } else {
         log.info("delta dump fired");
         this.itemDumpService.deltaDump(Integer.valueOf(15));
         log.info("delta dump end");
      }
   }
}

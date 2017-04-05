package io.terminus.parana.web.msg.job;

import com.google.common.base.Throwables;
import io.terminus.parana.web.msg.job.MsgJobService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping({"/api/msg/job"})
public class MsgJobs {
   private static final Logger log = LoggerFactory.getLogger(MsgJobs.class);
   private final MsgJobService msgJobService;
   private final Integer BATCHSIZE = Integer.valueOf(100);

   @Autowired
   public MsgJobs(MsgJobService msgJobService) {
      this.msgJobService = msgJobService;
   }

   @Scheduled(
      cron = "0 0/1 * * * ?"
   )
   @RequestMapping({"/dispatchMessages"})
   @ResponseBody
   public void dispatchMessages() {
      log.info("dispatch messages start...");

      try {
         Long cnt = this.msgJobService.batchSendMessages(this.BATCHSIZE);
         log.info("now have [{}] messages to dispatch!", cnt);
         log.info("dispatch messages end...");
      } catch (Exception var2) {
         log.error("dispatch message failed, cause={}", Throwables.getStackTraceAsString(var2));
      }

   }

   @Scheduled(
      cron = "0 0/5 * * * ?"
   )
   @RequestMapping({"/closeMessages"})
   @ResponseBody
   public void closeMessages() {
      log.info("close messages begin...");

      try {
         Long cnt = this.msgJobService.closeMessages(this.BATCHSIZE);
         log.info("now have [{}] messages to closed!", cnt);
         log.info("close messages end...");
      } catch (Exception var2) {
         log.error("close message failed, cause={}", Throwables.getStackTraceAsString(var2));
      }

   }
}

package cn.blmdz.wolf.web.pay.controller;

import java.util.concurrent.TimeUnit;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.google.common.base.Stopwatch;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.web.pay.service.SyncThirdTransReadService;

@Controller
@RequestMapping({"/api"})
public class Pays {
   private static final Logger log = LoggerFactory.getLogger(Pays.class);
   final DateTimeFormatter DT = DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss");
   @Autowired
   private SyncThirdTransReadService syncAlipayTrans;
   private static final int PAGE_SIZE = 100;

   @RequestMapping({"/alipayTrans/sync"})
   @ResponseBody
   public Boolean loadByDateRange(@RequestParam(
   value = "start",
   required = false
) String startIn, @RequestParam(
   value = "end",
   required = false
) String endIn, @RequestParam(
   value = "company",
   required = false
) String com) {
      DateTime now = DateTime.now();
      DateTime start = Strings.isNullOrEmpty(startIn)?now.minusDays(1):this.DT.parseDateTime(startIn);
      DateTime end = Strings.isNullOrEmpty(endIn)?now:this.DT.parseDateTime(endIn);
      if(end.compareTo(start) <= 0) {
         return Boolean.valueOf(false);
      } else {
         try {
            log.info("loadByDateRange sync begin");

            Stopwatch watch;
            for(watch = Stopwatch.createStarted(); end.compareTo(start) > 0; start = start.plusDays(1)) {
               Integer pageNo = Integer.valueOf(1);
               DateTime limit = start.plusDays(1).compareTo(end) < 0?start.plusDays(1):end;

               while(true) {
                  Response<Integer> loadedResp = this.syncAlipayTrans.syncAlipayTrans(start.toDate(), limit.toDate(), pageNo, Integer.valueOf(100));
                  log.info("Alipay trans loaded:[{}~{}] length:[{}]", new Object[]{this.DT.print(start), this.DT.print(limit), loadedResp.getResult()});
                  pageNo = Integer.valueOf(pageNo.intValue() + 1);
                  if(!loadedResp.isSuccess()) {
                     log.error(loadedResp.getError());
                     break;
                  }

                  if(((Integer)loadedResp.getResult()).intValue() < 100) {
                     break;
                  }
               }
            }

            watch.stop();
            log.info("loadByDateRanger sync end, cost time: {} ms", Long.valueOf(watch.elapsed(TimeUnit.MILLISECONDS)));
         } catch (Exception var13) {
            log.error("Error raise when loadLastDayOfHaier,cause:{}", Throwables.getStackTraceAsString(var13));
         }

         return Boolean.TRUE;
      }
   }
}

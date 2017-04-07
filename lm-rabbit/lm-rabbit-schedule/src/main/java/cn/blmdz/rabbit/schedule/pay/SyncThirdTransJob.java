package cn.blmdz.rabbit.schedule.pay;

import java.util.Date;
import java.util.concurrent.TimeUnit;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Configurable;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.google.common.base.Stopwatch;
import com.google.common.base.Throwables;

import cn.blmdz.aide.pay.channel.wechatpay.request.WxDownloadBillRequest;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.schedule.service.SyncThirdTransService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/21/16
 * Time: 7:40 PM
 */

@Slf4j
@Component
@Configurable
@EnableScheduling
public class SyncThirdTransJob {

    @Autowired
    private SyncThirdTransService syncThirdTransService;

    private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");

    private DateTimeFormatter NUM_DT = DateTimeFormat.forPattern("YYYYMMdd");

    private static final int HOURS_BACK = 3;

    private static final int PAGE_SIZE = 100;

    //################################### 支付宝账务 start #################################


    @Scheduled(cron = "0 */1 *  * * * ")
    public void syncAliapyTrans(){
        log.info("[CRON-JOB] [SYNC-ALIPAY-TRANS] begin {}", DFT.print(DateTime.now()));

        DateTime now = DateTime.now();
        Date start = now.minusHours(HOURS_BACK).toDate();
        Date end = now.toDate();
        Integer pageNo = 1;
        Stopwatch watch = Stopwatch.createStarted();
        try {
            log.info("[CRON-JOB] loadByHoursInterval job begin");
            while (true) {
                Response<Integer> loadedResp = syncThirdTransService.syncAlipayTrans(start, end, pageNo, PAGE_SIZE);
                log.info("galaxy alipay trans loaded:[{}~{}] length:[{}]", DFT.print(start.getTime()), DFT.print(end.getTime()), loadedResp.getResult());
                pageNo++;
                if (!loadedResp.isSuccess() || loadedResp.getResult() < PAGE_SIZE) break;
            }
        } catch (Exception e) {
            log.error("Error sync alipay trans ,cause:{}", Throwables.getStackTraceAsString(e));
        }

        watch.stop();
        log.info("[CRON-JOB] [SYNC-ALIPAY-TRANS] done at {} cost {} ms", DFT.print(DateTime.now()), watch.elapsed(TimeUnit.MILLISECONDS));

    }

    /**
     * 下载daqihui微信账户账务明细
     */
    //@Scheduled(cron = "0 */1 *  * * * ")
    public void syncWechatpayTransByDay() {


        log.info("[CRON-JOB][wechatpay bill detail download] loadByDay job begin");
        Stopwatch watch = Stopwatch.createStarted();
        Response<Integer> loaded;
        DateTime now = DateTime.now();

        String yesterday = NUM_DT.print(now.minusDays(1));
        loaded = syncThirdTransService.syncWechatpayAccountDetails(now.minusDays(1).toDate(), WxDownloadBillRequest.BillType.ALL);
        log.info("[galaxy wechatpay account load job] finish date:[{}], length:[{}]", yesterday, loaded.getResult());

        String beforeYesterday = NUM_DT.print(now.minusDays(2));
        loaded = syncThirdTransService.syncWechatpayAccountDetails(now.minusDays(2).toDate(), WxDownloadBillRequest.BillType.ALL);
        log.info("[galaxy wechatpay account load job] finish date:[{}], length:[{}]", beforeYesterday, loaded.getResult());

        watch.stop();
        log.info("[CRON-JOB][wechatpay bill detail download] job end, cost time: {} ms", watch.elapsed(TimeUnit.MILLISECONDS));
    }

}

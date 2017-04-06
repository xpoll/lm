package io.terminus.galaxy.schedule.pay;

import com.google.common.base.Stopwatch;
import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.galaxy.schedule.service.SyncThirdTransService;
import io.terminus.lib.pay.channel.wechatpay.request.WxDownloadBillRequest;
import lombok.extern.slf4j.Slf4j;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Configurable;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.concurrent.TimeUnit;

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



    /**
     * 下载银联账务明细
     */
    //@Scheduled(cron = "0 */1 *  * * * ")
    public void syncUnionPayTransByDay() {


        log.info("[CRON-JOB][unionpay bill detail download] loadByDay job begin");
        Stopwatch watch = Stopwatch.createStarted();
        Response<Integer> loaded;
        DateTime now = DateTime.now();

        String yesterday = NUM_DT.print(now.minusDays(1));
        loaded = syncThirdTransService.syncUnionPayTrans(now.minusDays(1).toDate());
        log.info("[unionpay account load job] finish date:[{}], length:[{}]", yesterday, loaded.getResult());

        watch.stop();
        log.info("[CRON-JOB][unionpay bill detail download] job end, cost time: {} ms", watch.elapsed(TimeUnit.MILLISECONDS));
    }


    /**
     * 按天获取平台快捷通账务信息
     */
    //@Scheduled(cron = "0 */1 *  * * * ")
    public void syncKjtpayTransByDay() {
        DateTime now = DateTime.now();
        Date date = now.minusDays(1).toDate();
        Integer pageNo = 1;
        try {
            log.info("[CRON-JOB] loadByDay kjtpay job begin");
            Stopwatch watch = Stopwatch.createStarted();
            while (true) {
                Response<Boolean> loadedResp = syncThirdTransService.syncKjtpayTrans(date, pageNo);
                if(!loadedResp.isSuccess()){
                    break;
                }
                pageNo++;
                if (!loadedResp.getResult()) break;
            }
            watch.stop();
            log.info("[CRON-JOB] loadByDay kjtpay job end, cost time: {} ms", watch.elapsed(TimeUnit.MILLISECONDS));
        } catch (Exception e) {
            log.error("Error raise when loadLastDay", e);
        }
    }
}

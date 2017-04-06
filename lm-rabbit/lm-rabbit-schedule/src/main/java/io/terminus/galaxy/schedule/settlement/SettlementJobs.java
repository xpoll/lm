package io.terminus.galaxy.schedule.settlement;

import com.google.common.base.Stopwatch;
import io.terminus.common.model.Response;
import io.terminus.galaxy.schedule.service.SettlementJobService;
import io.terminus.galaxy.schedule.service.SettlementRedisJobService;
import lombok.extern.slf4j.Slf4j;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Configurable;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/22/16
 * Time: 10:33 PM
 */
@Slf4j
@Component
@Configurable
@EnableScheduling
public class SettlementJobs {


    @Autowired
    private SettlementJobService settlementJobService;
    @Autowired
    private SettlementRedisJobService settlementRedisJobService;

    private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");


    //################################### 增量 生成支付渠道明细 start #################################



    @Scheduled(cron = "0 */5 *  * * * ")
    public void generateChannelSummaryDetail(){

        //处理redis
        handlePayChannelRedisIds();


        log.info("[CRON-JOB] [GENERATE-CHANNEL-SUMMARY-DETAIL] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementJobService.generateChannelSummaryDetail();
        if(!response.isSuccess()){
            log.error("generate channel sum detail fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[CRON-JOB] [GENERATE-CHANNEL-SUMMARY-DETAIL] done at {} cost {} ms", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.MILLISECONDS));

    }



    private void handlePayChannelRedisIds(){


        log.info("[CRON-JOB] [HANDLE-PAY-CHANNEL-REDIS-IDS] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementRedisJobService.handlePayChannelRedisIds();
        if(!response.isSuccess()){
            log.error("handle pay channel redis ids fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[CRON-JOB] [HANDLE-PAY-CHANNEL-REDIS-IDS] done at {} cost {} ms", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.MILLISECONDS));

    }

    //################################### 增量 支付渠道明细对账 start #################################
    @Scheduled(cron = "0 */1 *  * * * ")
    public void check(){

        //处理redis
        handlePayChannelDetailCheckRedisIds();

        log.info("[CHECK-ACCOUNTS] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementJobService.checkAccounts();
        if(!response.isSuccess()){
            log.error("check account fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[CHECK-ACCOUNTS] done at {} cost {}", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.SECONDS));

    }



    private void handlePayChannelDetailCheckRedisIds(){


        log.info("[CRON-JOB] [HANDLE-PAY-CHANNEL-DETAIL-CHECK-REDIS-IDS] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementRedisJobService.handlePayChannelDetailCheckRedisIds();
        if(!response.isSuccess()){
            log.error("handle pay channel detail check redis ids fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[CRON-JOB] [HANDLE-PAY-CHANNEL-DETAIL-CHECK-REDIS-IDS] done at {} cost {} ms", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.MILLISECONDS));

    }


    //################################### 增量 拉取交易订单资金流转 start #################################

    @Scheduled(cron = "0 */4 *  * * * ")
    public void syncOrderMoneyFlow(){

        //处理redis
        handleOrderMoneyFlowRedisIds();
        log.info("[SYNC-ORDER-MONEY-FLOW] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementJobService.syncOrderMondyFlow();
        if(!response.isSuccess()){
            log.error("sync order money flow fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[SYNC-ORDER-MONEY-FLOW] done at {} cost {}", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.SECONDS));

    }


    private void handleOrderMoneyFlowRedisIds(){


        log.info("[CRON-JOB] [HANDLE-ORDER-MONEY-FLOW-REDIS-IDS] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementRedisJobService.handleOrderMoneyFlowRedisIds();
        if(!response.isSuccess()){
            log.error("handle order money flow redis ids fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[CRON-JOB] [HANDLE-ORDER-MONEY-FLOW-REDIS-IDS] done at {} cost {} ms", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.MILLISECONDS));

    }


    //################################### 拉取交易完成的订单 start #################################

    @Scheduled(cron = "0 */1 *  * * * ")
    public void syncOrderFinshInfo(){

        //处理redis
        handleOrderFinishInfoRedisIds();

        log.info("[SYNC-ORDER-FINISH-INFO] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementJobService.syncOrderFinishInfo();
        if(!response.isSuccess()){
            log.error("sync order finish info fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[SYNC-ORDER-FINISH-INFO] done at {} cost {}", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.SECONDS));

    }

    private void handleOrderFinishInfoRedisIds(){


        log.info("[CRON-JOB] [HANDLE-ORDER-FINISH-INFO-REDIS-IDS] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementRedisJobService.handleOrderFinishInfoRedisIds();
        if(!response.isSuccess()){
            log.error("handle order finish info redis ids fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[CRON-JOB] [HANDLE-ORDER-FINISH-INFO-REDIS-IDS] done at {} cost {} ms", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.MILLISECONDS));

    }


    //###################################  结算单对账 start #################################

    @Scheduled(cron = "0 */1 *  * * * ")
    public void handleSettlementCheck(){

        //处理redis
        handleSettlementCheckRedisIds();

        log.info("[HANDLE-SETTLEMENT-CHECK] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementJobService.handleSettlementCheckStatus();
        if(!response.isSuccess()){
            log.error("handle settlement check fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[HANDLE-SETTLEMENT-CHECK] done at {} cost {}", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.SECONDS));

    }


    private void handleSettlementCheckRedisIds(){


        log.info("[CRON-JOB] [HANDLE-SETTLEMENT-CHECK-REDIS-IDS] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementRedisJobService.handleSettlementCheckRedisIds();
        if(!response.isSuccess()){
            log.error("handle settlement chek redis ids fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[CRON-JOB] [HANDLE-SETTLEMENT-CHECK-REDIS-IDS] done at {} cost {} ms", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.MILLISECONDS));

    }

    //################################### 售后对账 start #################################
    @Scheduled(cron = "0 */1 *  * * * ")
    public void checkAfterSales(){


        //处理redis
        handleAfterSettlementRedisIds();

        log.info("[CHECK-AFTER-SALES-SETTLEMENT] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementJobService.checkAfterSalesSettlement();
        if(!response.isSuccess()){
            log.error("check after sales settlement fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[CHECK-AFTER-SALES-SETTLEMENT] done at {} cost {}", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.SECONDS));

    }


    private void handleAfterSettlementRedisIds(){


        log.info("[CRON-JOB] [HANDLE-AFTER-SETTLEMENT-REDIS-IDS] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();

        Response<Boolean> response = settlementRedisJobService.handleAfterSettlementRedisIds();
        if(!response.isSuccess()){
            log.error("handle after settlement redis ids fail,error:{}",response.getError());
        }

        stopwatch.stop();
        log.info("[CRON-JOB] [HANDLE-AFTER-SETTLEMENT-REDIS-IDS] done at {} cost {} ms", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.MILLISECONDS));

    }



    //################################### 店铺日汇总 start #################################

    /**
     * todo 手动入口 1、指定某一天的日汇总 2、指定日期范围内的日汇总（for循环执行每天的）
     * todo 待优化：查出日汇总表中最近一条汇总的汇总日期和当前日期比较（防止服务down多天），如果汇总存在则取汇总日期和当前时间比较，如果最近汇总不存在记录（第一次执行）
     * todo 则去结算基础表中找最早的一条完成记录中的完成时间和当前日期比较（也许汇总job晚于基础数据job好多天）如果存在
     * todo 则把前些天的都生成出来，如果不存在则说明没有需要汇总的数据
     */


    //店铺日汇总
    @Scheduled(cron = "0 */1 *  * * * ")
    public void shopDailySummary() {

        log.info("[DAILY-SUMMARY-SHOP-DAILY] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();
        Response<Boolean> response = settlementJobService.generateShopDailySum();
        if(!response.isSuccess()){
            log.error("handle seller daily summary fail,error:{}",response.getError());
        }
        stopwatch.stop();
        log.info("[CRON-JOB] [DAILY-SUMMARY-SHOP-DAILY] done at {} cost {} ms", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.MILLISECONDS));

    }


    //平台日汇总
    @Scheduled(cron = "0 */1 *  * * * ")
    public void platformDailySummary() {

        log.info("[DAILY-SUMMARY-PLATFORM-DAILY] begin {}", DFT.print(DateTime.now()));

        Stopwatch stopwatch = Stopwatch.createStarted();
        Response<Boolean> response = settlementJobService.generatePlatformDailySum();
        if(!response.isSuccess()){
            log.error("handle platform daily summary fail,error:{}",response.getError());
        }
        stopwatch.stop();
        log.info("[CRON-JOB] [DAILY-SUMMARY-PLATFORM-DAILY] done at {} cost {} ms", DFT.print(DateTime.now()), stopwatch.elapsed(TimeUnit.MILLISECONDS));

    }






}

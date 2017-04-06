package io.terminus.galaxy.schedule.service;

import com.google.common.base.Function;
import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.common.utils.BeanMapper;
import io.terminus.common.utils.Splitters;
import io.terminus.galaxy.order.model.OrderFinishInfo;
import io.terminus.galaxy.order.model.OrderMoneyFlow;
import io.terminus.galaxy.order.service.OrderSettlementReadService;
import io.terminus.galaxy.order.service.OrderSettlementWriteService;
import io.terminus.galaxy.settlement.config.SettlementSetting;
import io.terminus.galaxy.settlement.dto.CommissionDto;
import io.terminus.galaxy.settlement.enums.CommissionBusinessType;
import io.terminus.galaxy.settlement.enums.SettlementRedisBusinessType;
import io.terminus.galaxy.settlement.model.SettlementSumOfDaily;
import io.terminus.galaxy.settlement.model.SettlementSumOfShopDaily;
import io.terminus.galaxy.settlement.service.GalaxySettlementReadService;
import io.terminus.galaxy.settlement.service.GalaxySettlementWriteService;
import io.terminus.pampas.engine.MessageSources;
import io.terminus.parana.config.ConfigCenter;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.model.SkuOrderRefund;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.pay.dto.ThirdPartyFeeDto;
import io.terminus.parana.pay.enums.PayChannelType;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.PayStage;
import io.terminus.parana.pay.model.TradePay;
import io.terminus.parana.pay.service.PayReadService;
import io.terminus.parana.pay.service.PayWriteService;
import io.terminus.parana.settlement.dto.SettlementSumOfDailyDto;
import io.terminus.parana.settlement.enums.*;
import io.terminus.parana.settlement.model.*;
import io.terminus.parana.settlement.service.SettlementReadService;
import io.terminus.parana.settlement.service.SettlementWriteService;
import io.terminus.parana.shop.model.Shop;
import jline.internal.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;

import static com.google.common.base.Preconditions.checkState;
import static io.terminus.common.utils.Arguments.isNull;
import static io.terminus.common.utils.Arguments.notNull;
import static io.terminus.lib.pay.utils.NumberSwitch.getTransFormRate;
import static io.terminus.lib.pay.utils.NumberSwitch.integerToLong;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/22/16
 * Time: 10:33 PM
 */
@Slf4j @Component
public class SettlementJobService {

    @Autowired
    private SettlementReadService settlementReadService;

    @Autowired
    private SettlementWriteService settlementWriteService;

    @Autowired
    private GalaxySettlementReadService galaxySettlementReadService;

    @Autowired
    private PayReadService payReadService;

    @Autowired
    private PayWriteService payWriteService;

    @Autowired
    private OrderSettlementReadService orderSettlementReadService;

    @Autowired
    private OrderSettlementWriteService orderSettlementWriteService;

    @Autowired
    private GalaxySettlementWriteService galaxySettlementWriteService;

    @Autowired
    private ConfigCenter configCenter;

    @Autowired
    private OrderReadService orderReadService;

    @Autowired

    static final Integer BATCH_SIZE = 100;     // 批处理数量


   /* @Autowired
    private MessageSources messageSources;*/

    public Response<Boolean> generateChannelSummaryDetail() {
        Response<Boolean> result = new Response<Boolean>();

        try {
            Response<List<Long>> payChannelIdsResult = galaxySettlementReadService.getSettlementReidsIds(SettlementRedisBusinessType.PAY_CHANNEL);
            checkState(payChannelIdsResult.isSuccess(), payChannelIdsResult.getError());

            List<Long> ids = payChannelIdsResult.getResult();

            if (CollectionUtils.isEmpty(ids)) {
                result.setResult(Boolean.TRUE);
                return result;
            }

            for (Long id : ids) {

                Response<PayChannel> payChannelRes = payReadService.findPayChannelById(id);
                if(!payChannelRes.isSuccess()){
                    log.error("query pay channel by id: {} fail,error:{}",id,payChannelRes.getError());
                    continue;
                }
                PayChannel channel = payChannelRes.getResult();
                PayChannelType type = PayChannelType.from(channel.getType());

                String code = type.equals(PayChannelType.PAID) ? channel.getPaymentCode() : channel.getBatchNo();
                //判断是否已经生成过
                Response<PayChannelDetail> detailRes = settlementReadService.findPayChannelDetails(channel.getChannel(), code, type.value());
                if(!detailRes.isSuccess()){
                    log.error("find sum pay channel detail fail where channel: {} code: {} type: {} , error:{}", channel.getChannel(), code, type, detailRes.getError());
                    continue;
                }
                //检测是记录过 如果记录过则跳过
                if(notNull(detailRes.getResult())){
                    log.warn("sum detail already exist where channel: {} code: {} type: {} skip", channel.getChannel(),code,type);
                    //更新paychannel状态
                    updatePayChannel(channel);
                    continue;
                }

                Response<PayStage> stageResponse = payReadService.findStageByPaymentCode(channel.getPaymentCode());
                if(!stageResponse.isSuccess()){
                    log.error("find pay stage by payment code: {} fail, error: {}",channel.getPaymentCode(),stageResponse.getError());
                    continue;
                }

                PayStage stage = stageResponse.getResult();
                PayChannelDetail detail = new PayChannelDetail();
                detail.setType(PayChannelTransType.from(channel.getBusinessType()).value());
                detail.setChannel(channel.getChannel());
                String innerchannel = getInnerChannel(channel.getChannel());
                if(Strings.isNullOrEmpty(innerchannel)){
                    continue;
                }
                detail.setInnerChannel(innerchannel);
                if(type.equals(PayChannelType.PAID)){

                    detail.setFee(integerToLong(channel.getFee()));
                }else {
                    detail.setFee(0l - integerToLong(channel.getFee()));
                }
                detail.setPaidAt(channel.getPaidAt());
                detail.setRefundAt(channel.getRefundAt());
                detail.setPaymentCode(channel.getPaymentCode());
                detail.setBatchNo(channel.getBatchNo());
                detail.setSystemNo(stage.getSystemNo());
                detail.setRefundOrderId(channel.getRefundOrderId());
                detail.setThirdPartyFee(0l);
                detail.setThirdPartyRate(0);
                detail.setTradeNo(channel.getTradeNo());
                detail.setCheckStatus(CheckStatus.WAIT_CHECK.value());

                Response<Boolean> response = settlementWriteService.createPayChannelDetail(detail);
                if(!response.isSuccess()){
                    log.error(response.getError());
                    continue;
                }
                //更新paychannel状态
                updatePayChannel(channel);


            }

            //redis中的ids处理完后删除key
            Response<Boolean> response = galaxySettlementWriteService.delSettlementRedisIds(SettlementRedisBusinessType.PAY_CHANNEL);
            if(!response.isSuccess()){
                log.error("delete settlement redis ids fail,error: {}",response.getError());
            }

            result.setResult(Boolean.TRUE);
        } catch (IllegalStateException e) {
            log.error("[CRON-JOB] [GENERATE-CHANNEL-SUMMARY-DETAIL] failed error:{} ", e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("[CRON-JOB] [GENERATE-CHANNEL-SUMMARY-DETAIL] failed, cause:{} ", Throwables.getStackTraceAsString(e));
            result.setError("generate.channel.sum.detail.fail");
        }
        return result;
    }


    private void updatePayChannel(PayChannel payChannel){
        PayChannel update = new PayChannel();
        update.setId(payChannel.getId());
        update.setIsCreatedDetail(1);
        Response<Boolean> updatePaychannelRes = payWriteService.updatePayChannel(update);
        if(!updatePaychannelRes.isSuccess()){
            log.error(updatePaychannelRes.getError());
        }
    }

    private String getInnerChannel(String channel){
        Response<String> channelRes =payReadService.findInnerChannelByChannel(channel);
        if(!channelRes.isSuccess()){
            log.error("find inner channel by channel:{} fail,error:{}",channel,channelRes.getError());
            return null;
        }
        return channelRes.getResult();
    }




    public Response<Boolean> checkAccounts() {
        Response<Boolean> result = new Response<Boolean>();

        try {
            Response<List<Long>> payChannelDetailIdsResult = galaxySettlementReadService.getSettlementReidsIds(SettlementRedisBusinessType.PAY_CHANNEL_DETAIL_CHECK);
            checkState(payChannelDetailIdsResult.isSuccess(), payChannelDetailIdsResult.getError());

            List<Long> ids = payChannelDetailIdsResult.getResult();

            if (CollectionUtils.isEmpty(ids)) {
                result.setResult(Boolean.TRUE);
                return result;
            }

            for (Long id : ids) {
                try {
                    Response<PayChannelDetail> detailRes = settlementReadService.findPayChannelDetailById(id);
                    if(!detailRes.isSuccess()){
                        log.error("find pay channel detail by id: {} fail, error: {}",id,detailRes.getError());
                        continue;
                    }
                    checkAccount(detailRes.getResult());
                }catch (IllegalStateException e){
                    log.error("check pay channel detail where id:{} fail,error:{}",id,e.getMessage());
                }
            }
            result.setResult(Boolean.TRUE);


            //redis中的ids处理完后删除key
            Response<Boolean> response = galaxySettlementWriteService.delSettlementRedisIds(SettlementRedisBusinessType.PAY_CHANNEL_DETAIL_CHECK);
            if(!response.isSuccess()){
                log.error("delete settlement redis ids fail,error: {}",response.getError());
            }

        } catch (IllegalStateException e) {
            log.error("[CHECK-ACCOUNT] failed error:{} ", e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("[CHECK-ACCOUNT] failed, cause:{} ", Throwables.getStackTraceAsString(e));
            result.setError("check.account.fail");
        }
        return result;
    }


    //对账  拿我们系统的账务（paychannel与第三方账务对账）
    private void checkAccount(PayChannelDetail detail){
        PayChannelTransType type = PayChannelTransType.from(detail.getType());
        checkState(Arguments.notNull(type),"pay.channel.sum.detail.type.invalid");
        switch (type){
            case PAID:
                checkOrderFee(detail);
                break;
            case REJECT_REFUND:
                checkRefundFee(detail);
                break;
            case AFTER_REFUND:
                checkRefundFee(detail);
                break;
        }
    }

    private void checkRefundFee(PayChannelDetail detail) {
        if(isOfflineRefund()){
            //线下退款不用对账，直接对账成功
            Response<Boolean> updateRes = settlementWriteService.updatePayChannelDetailStatus(detail.getId(), CheckStatus.CHECK_SUCCESS);
            checkState(updateRes.isSuccess(),updateRes.getError());
            return;
        }
        Response<List<ThirdPartyFeeDto>> dtosRes = payReadService.getThirdPartyFeeReverse(detail.getPaymentCode(), detail.getInnerChannel());
        checkState(dtosRes.isSuccess(),dtosRes.getError());
        List<ThirdPartyFeeDto> dtos = dtosRes.getResult();
        if(CollectionUtils.isEmpty(dtos)){
            checkFee(null,detail);
            return;
        }
        //一条退款
        if(dtos.size()==1){
            ThirdPartyFeeDto dto = dtos.get(0);
            checkFee(dto,detail);
        }else {
            //多条退款 只能通过判断本账务的退款金额是否在退款集合中
            List<Long> fees = Lists.transform(dtos, new Function<ThirdPartyFeeDto, Long>() {
                @Nullable
                @Override
                public Long apply(ThirdPartyFeeDto input) {
                    return input.getFee();
                }
            });
            if(fees.contains(Math.abs(detail.getFee()))){
                Response<Boolean> updateRes = settlementWriteService.updatePayChannelDetailStatus(detail.getId(), CheckStatus.CHECK_SUCCESS);
                checkState(updateRes.isSuccess(),updateRes.getError());
            }else {
                Response<Boolean> updateRes = settlementWriteService.updatePayChannelDetailStatus(detail.getId(), CheckStatus.CHECK_FAIL);
                checkState(updateRes.isSuccess(),updateRes.getError());
            }

        }

    }

    private void checkOrderFee(PayChannelDetail detail) {
        Response<ThirdPartyFeeDto> dtoRes = payReadService.getThirdPartyFeeForward(detail.getPaymentCode(), detail.getInnerChannel());
        if(dtoRes.isSuccess()){

            if(isNull(dtoRes.getResult())){
                checkFee(null, detail);
                return;
            }
            ThirdPartyFeeDto dto = dtoRes.getResult();
            checkFee(dto,detail);
        }else {
            log.error("get third party fee where payment code:{},channel:{} fail,error:{}",detail.getPaymentCode(), detail.getInnerChannel(),dtoRes.getError());
        }
    }

    private void checkFee( ThirdPartyFeeDto dto,PayChannelDetail detail){
        if(isNull(dto)){
            Response<Boolean> updateRes = settlementWriteService.updatePayChannelDetailStatus(detail.getId(), CheckStatus.WAIT_THRID_ACCOUNTS);
            checkState(updateRes.isSuccess(), updateRes.getError());
            return;
        }
        if(dto.getFee().equals(Math.abs(detail.getFee()))){
            PayChannelDetail update = new PayChannelDetail();
            update.setId(detail.getId());
            update.setCheckedAt(new Date());
            update.setThirdPartyFee(isNull(dto.getThirdPartyFee())?0l:dto.getThirdPartyFee());
            update.setThirdPartyRate(isNull(dto.getThirdPartyRate())?0:getTransFormRate(dto.getThirdPartyRate()));
            update.setCheckStatus(CheckStatus.CHECK_SUCCESS.value());
            Response<Boolean> updateRes = settlementWriteService.updatePayChannelDetail(update);
            checkState(updateRes.isSuccess(),updateRes.getError());
        }else {
            Response<Boolean> updateRes = settlementWriteService.updatePayChannelDetailStatus(detail.getId(), CheckStatus.CHECK_FAIL);
            checkState(updateRes.isSuccess(),updateRes.getError());
        }
    }

    private Boolean isOfflineRefund(){

        if (!configCenter.get(SettlementSetting.REFUND_TYPE).isPresent()) {
            throw new ServiceException("config."+SettlementSetting.REFUND_TYPE+".not.exist");
        }

        String type = configCenter.get(SettlementSetting.REFUND_TYPE).get();
        if(!Strings.isNullOrEmpty(type)) {
            if (type.equals("2")) {
                return true;
            }else {
                return false;
            }
        }
        return false;
    }




    public Response<Boolean> syncOrderMondyFlow() {
        Response<Boolean> result = new Response<Boolean>();

        try {
            Response<List<Long>> orderMoneyFlowIdsResult = galaxySettlementReadService.getSettlementReidsIds(SettlementRedisBusinessType.ORDER_MONEY_FLOW);
            checkState(orderMoneyFlowIdsResult.isSuccess(), orderMoneyFlowIdsResult.getError());

            List<Long> ids = orderMoneyFlowIdsResult.getResult();

            if (CollectionUtils.isEmpty(ids)) {
                result.setResult(Boolean.TRUE);
                return result;
            }

            for (Long flowId : ids) {
                try {

                    Response<OrderMoneyFlow> flowRes = orderSettlementReadService.findOrderMoneyFlowById(flowId);
                    if(!flowRes.isSuccess()){
                        log.error("find order money flow by id: {} fail,error: {}",flowId,flowRes.getError());
                        continue;
                    }

                    flowToSettlement(flowRes.getResult());
                }catch (IllegalStateException e){
                    log.error("sync order money flow to settlement where id:{} fail,error:{}",flowId,e.getMessage());
                }
            }
            result.setResult(Boolean.TRUE);

            //redis中的ids处理完后删除key
            Response<Boolean> response = galaxySettlementWriteService.delSettlementRedisIds(SettlementRedisBusinessType.ORDER_MONEY_FLOW);
            if(!response.isSuccess()){
                log.error("delete settlement redis ids fail,error: {}",response.getError());
            }

        } catch (IllegalStateException e) {
            log.error("[SYNC-ORDER-MONEY-FLOW] failed error:{} ", e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("[SYNC-ORDER-MONEY-FLOW] failed, cause:{} ", Throwables.getStackTraceAsString(e));
            result.setError("sync.order.money.flow.fail");
        }
        return result;
    }


    //正向的订单要同时创建优惠券明细
    //售后退款要同时创建售后账务
    private void flowToSettlement(OrderMoneyFlow flow){
        BalanceSettlement settlement = new BalanceSettlement();

        BeanMapper.copy(flow,settlement);

        Response<BalanceSettlement> existRes = settlementReadService.findBalanceSettlementByOrderIdAndType(settlement.getOrderId(), settlement.getType());
        if(!existRes.isSuccess()){
            log.error(existRes.getError());
            return;
        }
        if(Arguments.notNull(existRes.getResult())){
            log.warn("order money flow has settlement so skip current where order id:{} type:{}",settlement.getOrderId(),settlement.getType());

        }else {
            Response<Boolean> settlementRes = createBalanceSettlement(settlement);
            if(!settlementRes.isSuccess()){
                log.error("create balance settlement fail error: "+settlementRes.getError());
                return;
            }
        }

        //结算存在的情况下 这里统一更新一把 放在某些情况下结算生成成功 而订单流水的状态更新失败 这里算是一个弥补吧
        Response<Boolean> updateRes = orderSettlementWriteService.updateOrderMoneyFlowSettlemented(flow.getId());
        if(!updateRes.isSuccess()){
            log.error(updateRes.getError());
        }
    }

    private Response<Boolean> createBalanceSettlement(BalanceSettlement settlement){

        BalanceType balanceType = BalanceType.from(settlement.getType());

        switch (balanceType){
            case PAID:
                return settlementWriteService.createPaidBalanceSettlement(settlement);
            case SALE_REFUND:
                return settlementWriteService.createSaleRefundBalanceSettlement(settlement);
            case AFTER_SALE_REFUND:
                AfterSalesSettlement salesSettlement = new AfterSalesSettlement();
                BeanMapper.copy(settlement,salesSettlement);
                salesSettlement.setRefundAmount(settlement.getFee());
                salesSettlement.setRefundAt(settlement.getTradeAt());
                CommissionDto dto = getShopCommission(settlement.getShopId(), settlement.getFee(),CommissionDetailType.AFTER_SALE_ORDER);
                salesSettlement.setCommission(dto.getCommisson());
                salesSettlement.setIsChecked(Boolean.FALSE);
                return settlementWriteService.createAfterSaleRefundBalanceSettlement(settlement,salesSettlement,dto.getCommissionDetails());
        }

        return Response.fail("balance.type.invalid");

    }


    private CommissionDto getShopCommission(Long shopId,Long fee,CommissionDetailType detailType){

        Response<CommissionDto> commissionRes = galaxySettlementReadService.countCommission(fee, CommissionType.SHOP, CommissionBusinessType.SHOP,detailType,shopId);
        checkState(commissionRes.isSuccess(),commissionRes.getError());

        return commissionRes.getResult();

    }




    public Response<Boolean> syncOrderFinishInfo() {
        Response<Boolean> result = new Response<Boolean>();

        try {
            Response<List<Long>> orderFinishInfoIdsResult = galaxySettlementReadService.getSettlementReidsIds(SettlementRedisBusinessType.ORDER_FINISH_INFO);
            checkState(orderFinishInfoIdsResult.isSuccess(), orderFinishInfoIdsResult.getError());

            List<Long> ids = orderFinishInfoIdsResult.getResult();

            if (CollectionUtils.isEmpty(ids)) {
                result.setResult(Boolean.TRUE);

                return result;
            }

            for (Long infoId : ids) {
                try {
                    Response<OrderFinishInfo> infoRes = orderSettlementReadService.findOrderFinishInfoById(infoId);
                    if(!infoRes.isSuccess()){
                        log.error("find order finish info by id: {} fail,error : {}",infoId,infoRes.getError());
                        continue;
                    }
                    infoToSettlement(infoRes.getResult());
                }catch (IllegalStateException e){
                    log.error("sync order finish info to settlement where id:{} fail,error:{}",infoId,e.getMessage());
                }
            }
            result.setResult(Boolean.TRUE);

            //redis中的ids处理完后删除key
            Response<Boolean> response = galaxySettlementWriteService.delSettlementRedisIds(SettlementRedisBusinessType.ORDER_FINISH_INFO);
            if(!response.isSuccess()){
                log.error("delete settlement redis ids fail,error: {}",response.getError());
            }

        } catch (IllegalStateException e) {
            log.error("[SYNC-ORDER-FINISH-INFO] failed error:{} ", e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("[SYNC-ORDER-FINISH-INFO] failed, cause:{} ", Throwables.getStackTraceAsString(e));
            result.setError("sync.order.finish.info.fail");
        }
        return result;
    }


    private void infoToSettlement(OrderFinishInfo info){
        OrderSettlement settlement = new OrderSettlement();
        BeanMapper.copy(info,settlement);

        Long refundAmount = getRefundAmount(settlement.getOrderId());
        settlement.setRefundFee(refundAmount);
        settlement.setIsChecked(Boolean.FALSE);
        //todo 是否需要判断是否合并支付
        settlement.setMergePaid(Boolean.FALSE);
        //todo 拆分discount
        settlement.setSellerDiscount(0L);
        settlement.setPlatformDiscount(0L);
        settlement.setReceivable(0L);

        CommissionDto dto = getShopCommission(settlement.getShopId(), settlement.getFee()-refundAmount,CommissionDetailType.FINISH_ORDER);
        settlement.setCommission(dto.getCommisson());

        Response<Boolean> settlementRes = settlementWriteService.createOrderSettlement(settlement,dto.getCommissionDetails());
        if(!settlementRes.isSuccess()){
            log.error(settlementRes.getError());
            return;
        }

        Response<Boolean> updateRes = orderSettlementWriteService.updateOrderFinishInfoSettlemented(info.getId());
        if(!updateRes.isSuccess()){
            log.error(updateRes.getError());
        }
    }


    //获取订单的退款金额
    private Long getRefundAmount(Long shopOrderId){

        Long refundAmount = 0L;
        List<SkuOrderRefund> refunds = getRefundOrder(shopOrderId,1);
        if(Arguments.isNullOrEmpty(refunds)){
            return refundAmount;
        }
        for(SkuOrderRefund refund : refunds){
            refundAmount+=refund.getRefundAmount();
        }
        return refundAmount;

    }

    private List<SkuOrderRefund> getRefundOrder(Long shopOrderId,Integer type){
        Response<List<SkuOrderRefund>> listRes = orderSettlementReadService.findRefundOrder(shopOrderId,type);
        checkState(listRes.isSuccess(),listRes.getError());
        List<SkuOrderRefund> refunds = listRes.getResult();
        return refunds;
    }

    public Response<Boolean> handleSettlementCheckStatus() {

        Response<Boolean> result = new Response<Boolean>();
        try {

            Response<List<Long>> orderSettlementIdsResult = galaxySettlementReadService.getSettlementReidsIds(SettlementRedisBusinessType.ORDER_SETTLEMENT_CHECK);
            checkState(orderSettlementIdsResult.isSuccess(), orderSettlementIdsResult.getError());

            List<Long> ids = orderSettlementIdsResult.getResult();

            if (CollectionUtils.isEmpty(ids)) {
                result.setResult(Boolean.TRUE);
                return result;
            }

            for (Long settlementId : ids) {

                Response<OrderSettlement> orderSettlementRes = settlementReadService.findOrderSettlementById(settlementId);
                if(!orderSettlementRes.isSuccess()){
                    log.error("find order settlement by id: {} fail,error: {}",settlementId,orderSettlementRes.getError());
                    continue;
                }
                try {

                    updateCheckStatus(orderSettlementRes.getResult());

                }catch (IllegalStateException e){
                    log.error("update settlement check status where id: {} fail,error: {}",settlementId,e.getMessage());
                    OrderSettlement st = new OrderSettlement();
                    st.setMemo("更新对账状态原因："+e.getMessage());
                    st.setId(settlementId);
                    updateSettlement(st);
                }catch (Exception e){
                    log.error("update settlement check status where id: {} fail,cause: {}",settlementId,Throwables.getStackTraceAsString(e));
                    OrderSettlement st = new OrderSettlement();
                    st.setMemo("更新对账状态失败");
                    st.setId(settlementId);
                    updateSettlement(st);
                }


            }

            result.setResult(Boolean.TRUE);

            //redis中的ids处理完后删除key
            Response<Boolean> response = galaxySettlementWriteService.delSettlementRedisIds(SettlementRedisBusinessType.ORDER_SETTLEMENT_CHECK);
            if(!response.isSuccess()){
                log.error("delete settlement redis ids fail,error: {}",response.getError());
            }

        } catch (Exception e) {
            log.error("handle settlement check status fail,cause:{}", Throwables.getStackTraceAsString(e));

        }
        return result;

    }




    private void updateCheckStatus(OrderSettlement settlement){

        TradePay tradePay = getTradePay(settlement.getSystemNo());
        //要考虑是否合并支付
        List<PayChannelDetail> details = getForwardPayChannelDetail(settlement.getSystemNo());
        if(!org.springframework.util.CollectionUtils.isEmpty(details)){
            //判断支付阶段数是否和正向对账明细记录数相等 即正向账务是否全部对上账
            Boolean payOver = Boolean.FALSE;
            if(tradePay.getStage().equals(details.size())){
                payOver = Boolean.TRUE;
            }
            Long thirdFee = 0l;
            for (PayChannelDetail detail : details){
                thirdFee+=detail.getThirdPartyFee();
            }

            Boolean refundOver = Boolean.TRUE;
            //获取售中退款单
            List<SkuOrderRefund> tracks = getRefundOrder(settlement.getOrderId(),1);
            for (SkuOrderRefund track : tracks){
                PayChannelDetail detail = getRefundPayChannelSumDetail(track.getId());
                if(isNull(detail)){
                    refundOver = Boolean.FALSE;
                    break;
                }
                if(!detail.getCheckStatus().equals(CheckStatus.CHECK_SUCCESS.value())){
                    refundOver = Boolean.FALSE;
                    break;
                }
            }
            if(payOver&&refundOver){
                thirdFee = divideThirdFee(settlement.getOrderId(), tradePay, thirdFee);
                OrderSettlement st = new OrderSettlement();
                st.setId(settlement.getId());
                st.setIsChecked(Boolean.TRUE);
                st.setCheckedAt(new Date());
                st.setMemo("对账完成可参与汇总了");
                st.setThirdPartyFee(thirdFee);
                updateSettlement(st);

            }else {
                OrderSettlement st = new OrderSettlement();
                st.setId(settlement.getId());
                st.setMemo("待对账");
                updateSettlement(st);
            }
        }else {
            OrderSettlement update = new OrderSettlement();
            update.setId(settlement.getId());
            update.setMemo("对账未完成，暂未拉取到第三方账务。");
            updateSettlement(update);
        }
    }



    /**
     * 划分手续费 并返回当前订单的手续费
     */
    private Long divideThirdFee(Long orderId,TradePay tradePay,Long thirdPartyFee) {
        if(tradePay.getMergePaid()){
            Map<Long,Integer> orderIdAndFee = Maps.newHashMap();
            List<Long> order_ids = Splitters.splitToLong(tradePay.getOrderIds(), Splitters.COMMA); //参与合并支付的订单id
            Integer totalFee =0;
            for(Long id :order_ids){
                Response<ShopOrder> orderRes = orderReadService.findShopOrderById(id);
                checkState(orderRes.isSuccess(),orderRes.getError());
                orderIdAndFee.put(id, orderRes.getResult().getFee());
                totalFee += orderRes.getResult().getFee();
            }
            return getOrderThirdPartFee(orderId, totalFee, thirdPartyFee, order_ids, orderIdAndFee);
        }else {
            return  thirdPartyFee;
        }
    }


    /**
     * 获取合并支付其中一个订单的手续费
     */
    private Long getOrderThirdPartFee(Long currentOrderId,Integer toatalFee, Long totalThirdPartFee, List<Long> orderIds, Map<Long, Integer> orderIdAndFee) {

        Map<Long, Long> orderIdAndThirdPartFee = Maps.newHashMap();
        //orderId必须是有序的
        Long lastSkuId = orderIds.get(orderIds.size()-1);
        Long totalAssigned = 0l;

        for(Long orderId : orderIds) {
            if(Objects.equal(orderId, lastSkuId)) {
                orderIdAndThirdPartFee.put(orderId, new BigDecimal(totalThirdPartFee).subtract(new BigDecimal(totalAssigned)).longValue());
            }else {
                Long price = new BigDecimal(orderIdAndFee.get(orderId)).multiply(new BigDecimal(totalThirdPartFee))
                        .divide(new BigDecimal(toatalFee),BigDecimal.ROUND_UP).longValue();
                totalAssigned += price;
                orderIdAndThirdPartFee.put(orderId,price);
            }
        }

        return orderIdAndThirdPartFee.get(currentOrderId);
    }



    private TradePay getTradePay(String systemNo){
        Response<TradePay> tradePayRes = payReadService.findTradePayBySystemNo(systemNo);
        checkState(tradePayRes.isSuccess(),tradePayRes.getError());
        return tradePayRes.getResult();
    }

    /**
     * 获取正向的支付渠道明细记录
     * @param systemNo 系统内部流水号
     * @return
     */
    private List<PayChannelDetail> getForwardPayChannelDetail(String systemNo){
        Response<List<PayChannelDetail>> detailRes = settlementReadService.findForwardPayChannelDetailBySystemNo(systemNo);
        checkState(detailRes.isSuccess(),detailRes.getError());
        return detailRes.getResult();

    }


    /**
     * 获取售中逆向的支付渠道明细记录
     * @param refundOrderId 退款单id
     * @return 汇总明细
     */
    private PayChannelDetail getRefundPayChannelSumDetail(Long refundOrderId){
        Response<PayChannelDetail> detailRes = settlementReadService.findPayChannelDetailByRefundOrderId(refundOrderId);
        checkState(detailRes.isSuccess(),detailRes.getError());
        return detailRes.getResult();

    }


    private void updateSettlement(OrderSettlement settlement){
        Response<Boolean> updateRes  = settlementWriteService.updateOrderSettlement(settlement);
        checkState(updateRes.isSuccess(),updateRes.getError());
    }




    public Response<Boolean> checkAfterSalesSettlement() {

        Response<Boolean> result = new Response<Boolean>();

        try {
            Response<List<Long>> afterSettlementIdsResult = galaxySettlementReadService.getSettlementReidsIds(SettlementRedisBusinessType.AFTER_SETTLEMENT_CHECK);
            checkState(afterSettlementIdsResult.isSuccess(), afterSettlementIdsResult.getError());

            List<Long> ids = afterSettlementIdsResult.getResult();

            if (CollectionUtils.isEmpty(ids)) {
                result.setResult(Boolean.TRUE);
                return result;
            }

            for (Long settlementId : ids) {
                Response<AfterSalesSettlement> afterSalesSettlementRes = settlementReadService.findAfterSalesSettlementById(settlementId);
                if(!afterSalesSettlementRes.isSuccess()){
                    log.error("find after sale settlement by id: {} fail,error: {}",settlementId,afterSalesSettlementRes.getError());
                    continue;
                }
                checkAfterSales(afterSalesSettlementRes.getResult());
            }
            result.setResult(Boolean.TRUE);

            //redis中的ids处理完后删除key
            Response<Boolean> response = galaxySettlementWriteService.delSettlementRedisIds(SettlementRedisBusinessType.AFTER_SETTLEMENT_CHECK);
            if(!response.isSuccess()){
                log.error("delete settlement redis ids fail,error: {}",response.getError());
            }

        } catch (IllegalStateException e) {
            log.error("[CHECK-AFTER-SALES-SETTLEMENT] failed error:{} ", e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("[[CHECK-AFTER-SALES-SETTLEMENT] failed, cause:{} ", Throwables.getStackTraceAsString(e));
            result.setError("check.after.sales.settlement.fail");
        }
        return result;
    }



    private void checkAfterSales(AfterSalesSettlement settlement){

        PayChannelDetail detail = getRefundPayChannelSumDetail(settlement.getOrderId());
        AfterSalesSettlement update = new AfterSalesSettlement();
        if(!isNull(detail)){
            update.setId(settlement.getId());
            if(detail.getCheckStatus().equals(CheckStatus.CHECK_SUCCESS.value())){
                update.setIsChecked(Boolean.TRUE);
                update.setCheckedAt(new Date());
            }
            Response<Boolean> updateRes = settlementWriteService.updateAfterSalesSettlement(update);
            checkState(updateRes.isSuccess(),updateRes.getError());
        }


    }


    public Response<Boolean> generateShopDailySum() {

        return this.generateShopDailySum(new Date());
    }


    public Response<Boolean> generateShopDailySum(Date date) {
        Response<Boolean> result = new Response<Boolean>();
        Date startAt = new DateTime(date).minusDays(1).withTimeAtStartOfDay().toDate();   //t日的0点
        Date endAt = new DateTime(date).withTimeAtStartOfDay().toDate();//t+1的0点;
        Date sumAt = new DateTime(date).minusDays(1).withTimeAtStartOfDay().toDate();//汇总日期

        try {

            Response<List<SettlementSumOfDailyDto>> dtoRes = settlementReadService.generateSettlementSumOfShopDaily(startAt, endAt);
            checkState(dtoRes.isSuccess(),dtoRes.getError());
            List<SettlementSumOfDailyDto> dailyDtos = dtoRes.getResult();
            if(Arguments.isNullOrEmpty(dailyDtos)){
                result.setResult(Boolean.TRUE);
                return result;
            }

            for (SettlementSumOfDailyDto dailyDto : dailyDtos){
                if(dailyDto.getOrderCount().equals(0)){
                    continue;
                }
                dailyDto.setSumAt(sumAt);
                //防止job执行多次 以最新一次为准
                Response<SettlementSumOfShopDaily> existRes = galaxySettlementReadService.findSettlementSumOfShopDailyByShopIdAndSumAt(dailyDto.getShopId(), sumAt);
                if(!existRes.isSuccess()){
                    log.error(existRes.getError());
                    continue;
                }
                if(notNull(existRes.getResult())){
                    log.warn("exist settlement sum of shop daily where sum at({})  shop id:({})", sumAt,dailyDto.getShopId());
                    Response<Boolean> isDelRes = galaxySettlementWriteService.delSettlementSumOfShopDaily(dailyDto.getShopId(), sumAt);
                    if(isDelRes.isSuccess()){
                        log.warn("success delete settlement sum of shop daily where sum at({})  shop id:({})", sumAt,dailyDto.getShopId());
                    }
                }

                SettlementSumOfShopDaily daily = new SettlementSumOfShopDaily();
                BeanMapper.copy(dailyDto,daily);

                Response<Boolean> response = galaxySettlementWriteService.createSettlementSumOfShopDaily(daily);
                if(!response.isSuccess()){
                    log.error(response.getError());
                }

            }

        } catch (IllegalStateException e) {
            log.error("[CRON-JOB] [SUMMARY-SHOP-SETTLEMENTS] failed error:{} ", e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("[CRON-JOB] [SUMMARY-SHOP-SETTLEMENTS] failed, cause:{} ", Throwables.getStackTraceAsString(e));
            result.setError("generate.shop.settlement.daily.sum.fail");
        }
        return result;
    }



    public Response<Boolean> generatePlatformDailySum() {

        return this.generatePlatformDailySum(new Date());
    }


    public Response<Boolean> generatePlatformDailySum(Date date) {
        Response<Boolean> result = new Response<Boolean>();
        Date startAt = new DateTime(date).minusDays(1).withTimeAtStartOfDay().toDate();   //t日的0点
        Date endAt = new DateTime(date).withTimeAtStartOfDay().toDate();//t+1的0点;
        Date sumAt = new DateTime(date).minusDays(1).withTimeAtStartOfDay().toDate();//汇总日期

        try {

            Response<SettlementSumOfDailyDto> dtoRes = settlementReadService.generateSettlementSumOfPlatformDaily(startAt, endAt);
            checkState(dtoRes.isSuccess(),dtoRes.getError());
            SettlementSumOfDailyDto dailyDto = dtoRes.getResult();
            if(isNull(dailyDto)){
                result.setResult(Boolean.TRUE);
                return result;
            }
            if(dailyDto.getOrderCount().equals(0)){
                result.setResult(Boolean.TRUE);
                return result;
            }

                dailyDto.setSumAt(sumAt);
                //防止job执行多次 以最新一次为准
                Response<SettlementSumOfDaily> existRes = galaxySettlementReadService.findSettlementSumOfDailyBySumAt(sumAt);
                if(!existRes.isSuccess()){
                    log.error(existRes.getError());
                    result.setError(existRes.getError());
                    return result;
                }
                if(notNull(existRes.getResult())){
                    log.warn("exist settlement sum of platform daily where sum at({})  shop id:({})", sumAt,dailyDto.getShopId());
                    Response<Boolean> isDelRes = galaxySettlementWriteService.delSettlementSumOfPlatformDaily(sumAt);
                    if(isDelRes.isSuccess()){
                        log.warn("success delete settlement sum of platform daily where sum at({})  shop id:({})", sumAt,dailyDto.getShopId());
                    }
                }

                SettlementSumOfDaily daily = new SettlementSumOfDaily();
                BeanMapper.copy(dailyDto,daily);

                Response<Boolean> response = galaxySettlementWriteService.createSettlementSumOfDaily(daily);
                if(!response.isSuccess()){
                    log.error(response.getError());
                }


        } catch (IllegalStateException e) {
            log.error("[CRON-JOB] [SUMMARY-PLATFROM-SETTLEMENTS] failed error:{} ", e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("[CRON-JOB] [SUMMARY-PLATFORM-SETTLEMENTS] failed, cause:{} ", Throwables.getStackTraceAsString(e));
            result.setError("generate.platform.settlement.daily.sum.fail");
        }
        return result;
    }



}

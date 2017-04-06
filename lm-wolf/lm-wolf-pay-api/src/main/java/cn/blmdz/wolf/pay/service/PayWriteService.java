package cn.blmdz.wolf.pay.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.pay.dto.PayStageInfo;
import cn.blmdz.wolf.pay.enums.ExceptionPayType;
import cn.blmdz.wolf.pay.enums.PayChannelStatus;
import cn.blmdz.wolf.pay.enums.TrackHandleStatus;
import cn.blmdz.wolf.pay.model.AlipayTrans;
import cn.blmdz.wolf.pay.model.ExceptionPayTrack;
import cn.blmdz.wolf.pay.model.KjtpayTrans;
import cn.blmdz.wolf.pay.model.OwnerPayChannel;
import cn.blmdz.wolf.pay.model.PayChannel;
import cn.blmdz.wolf.pay.model.PayStage;
import cn.blmdz.wolf.pay.model.TradePay;
import cn.blmdz.wolf.pay.model.UnionPayTrans;
import cn.blmdz.wolf.pay.model.WechatpayTrans;

public interface PayWriteService {
   Response updateStageFee(String var1, Map<Integer, Integer> var2);

   Response updatePayStage(PayStage var1);

   Response createPayStage(PayStage var1);

   Response createTradePay(TradePay var1);

   Response updateTradePay(TradePay var1);

   Response createTradePayWithStage(TradePay var1, List<PayStageInfo> var2, Date var3);

   Response updateTradePayStatus(String var1, Integer var2);

   Response createPayChannel(PayChannel var1);

   Response updatePayChannel(PayChannel var1);

   Response createPayChannelForRefund(PayChannel var1);

   Response updatePayStatus(PayChannel var1);

   Response createOwnerPayChannel(OwnerPayChannel var1);

   Response updateOwnerPayChannel(OwnerPayChannel var1);

   Response updatePayChannelStatusForRefund(String var1, PayChannelStatus var2, PayChannelStatus var3, Date var4);

   Response createExceptionPayTracks(ExceptionPayTrack var1);

   Response createExceptionPayTracks(String var1, ExceptionPayType var2);

   Response updateExceptionPayTrackStatus(Long var1, TrackHandleStatus var2);

   Response createAlipayTrans(AlipayTrans var1);

   Response createWechatPayTrans(WechatpayTrans var1);

   Response createUnionpayTrans(UnionPayTrans var1);

   Response createKjtpayTrans(KjtpayTrans var1);
}

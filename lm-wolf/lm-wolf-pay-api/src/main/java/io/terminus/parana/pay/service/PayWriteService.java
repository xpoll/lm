package io.terminus.parana.pay.service;

import io.terminus.common.model.Response;
import io.terminus.parana.pay.enums.ExceptionPayType;
import io.terminus.parana.pay.enums.PayChannelStatus;
import io.terminus.parana.pay.enums.TrackHandleStatus;
import io.terminus.parana.pay.model.AlipayTrans;
import io.terminus.parana.pay.model.ExceptionPayTrack;
import io.terminus.parana.pay.model.KjtpayTrans;
import io.terminus.parana.pay.model.OwnerPayChannel;
import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.PayStage;
import io.terminus.parana.pay.model.TradePay;
import io.terminus.parana.pay.model.UnionPayTrans;
import io.terminus.parana.pay.model.WechatpayTrans;
import java.util.Date;
import java.util.List;
import java.util.Map;

public interface PayWriteService {
   Response updateStageFee(String var1, Map var2);

   Response updatePayStage(PayStage var1);

   Response createPayStage(PayStage var1);

   Response createTradePay(TradePay var1);

   Response updateTradePay(TradePay var1);

   Response createTradePayWithStage(TradePay var1, List var2, Date var3);

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

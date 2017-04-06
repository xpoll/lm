package cn.blmdz.wolf.pay.service;

import java.util.Map;

import javax.validation.constraints.NotNull;

import cn.blmdz.home.common.model.Response;

public interface PayReadService {
	Response isAllStagePaid(String var1);

	Response isMultiplePay(String var1);

	Response findTradePayBySystemNo(String var1);

	Response findTradePayByTradeNo(String var1);

	Response findTradePayById(Long var1);

	Response findPayChannelById(Long var1);

	Response validIsExistPayChannelByStageIdAndChannel(Long var1, String var2);

	Response findPayChannelByTradeNoForPaid(String var1);

	Response findPayChannelByPaymentCodeForPaid(String var1);

	Response findPayChannelByBatchNoForRefund(String var1);

	Response findPayChannelByStageIdForPaid(Long var1);

	Response findPayChannelByStageIdAndChannel(Long var1, String var2);

	Response pagingNeedGenerateChannelDetails(Integer var1, Integer var2);

	Response findRefundByRefundIdAndChannel(Long var1, String var2);

	Response findPayStageById(Long var1);

	Response findPayStagesByPayId(Long var1);

	Response findPayStagesBySystemNo(String var1);

	Response findRecentStageBySystemNo(String var1);

	Response findStageByPaymentCode(String var1);

	Response checkNeedCreateTradePay(String var1, Boolean var2, Integer var3);

	Response findOwnerPayChannelByOwnerIdAndType(Long var1, Integer var2);

	Response findPayInfo(String var1, Integer var2);

	Response pagingTimeoutPayTracks(Integer pageNo, Integer pageSize, Map<String, Object> criteria);

	Response getTimeoutPayTrackById(Long var1);

	Response getThirdPartyFeeForward(@NotNull(message = "payment.code.is.null") String var1,
			@NotNull(message = "channel.is.null") String var2);

	Response getThirdPartyFeeReverse(@NotNull(message = "payment.code.is.null") String var1,
			@NotNull(message = "channel.is.null") String var2);

	Response findAlipayTransByTradeNo(String var1);

	Response findWechatpayTransByTransactionId(String var1);

	Response findInnerChannelByChannel(String var1);
}

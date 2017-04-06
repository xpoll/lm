package cn.blmdz.wolf.web.pay.service;

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.JavaType;
import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.eventbus.EventBus;
import com.thoughtworks.xstream.XStream;

import cn.blmdz.aide.file.ImageServer;
import cn.blmdz.aide.pay.channel.alipay.dto.RedirectInfo;
import cn.blmdz.aide.pay.channel.alipay.enums.RefundInterfaceType;
import cn.blmdz.aide.pay.channel.alipay.manager.AlipayManager;
import cn.blmdz.aide.pay.channel.alipay.request.AlipaySyncResponse;
import cn.blmdz.aide.pay.channel.alipay.request.AlipayToken;
import cn.blmdz.aide.pay.channel.alipay.request.RefundRequest;
import cn.blmdz.aide.pay.channel.wechatpay.HttpsUtil;
import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPayRefundResponse;
import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPrePayResponse;
import cn.blmdz.aide.pay.channel.wechatpay.manager.WechatpayManager;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxRequest;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxToken;
import cn.blmdz.aide.pay.utils.GenerateRandom;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.Joiners;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.wolf.file.util.FileUtil;
import cn.blmdz.wolf.pay.dto.PayStageInfo;
import cn.blmdz.wolf.pay.enums.PayChannelBusinessType;
import cn.blmdz.wolf.pay.enums.PayChannelStatus;
import cn.blmdz.wolf.pay.enums.PayChannelType;
import cn.blmdz.wolf.pay.model.PayChannel;
import cn.blmdz.wolf.pay.model.PayStage;
import cn.blmdz.wolf.pay.model.TradePay;
import cn.blmdz.wolf.pay.service.PayReadService;
import cn.blmdz.wolf.pay.service.PayWriteService;
import cn.blmdz.wolf.web.pay.dto.AlipaySettingsDto;
import cn.blmdz.wolf.web.pay.dto.KjtSettingsDto;
import cn.blmdz.wolf.web.pay.dto.UnionSettingsDto;
import cn.blmdz.wolf.web.pay.dto.WechatSettingsDto;
import cn.blmdz.wolf.web.pay.event.RefundedEvent;
import cn.blmdz.wolf.web.pay.util.QrHelper;

@Service
public class DefaultPayWebService implements PayWebService {
	private static final Logger log = LoggerFactory.getLogger(DefaultPayWebService.class);
	@Autowired
	private PayWriteService payWriteService;
	@Autowired
	private PayReadService payReadService;
	@Autowired
	private ImageServer imageServer;
	@Autowired
	private PaySettingsProvider paySettingsProvider;
	@Value("${pay.debug:false}")
	private String payDebug;
	@Autowired
	private EventBus eventBus;
	private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyyMMddHHmmss");
	private static XStream xStream = new XStream();
	private static final JsonMapper JSON_MAPPER = JsonMapper.nonDefaultMapper();
	private static final JavaType INSTALMENT_DATA = JSON_MAPPER.createCollectionType(Map.class,
			new Class[] { String.class, String.class });

	public Response refundRequest(Long refundTrackId, Integer orderType, String tradeNo, Integer refundAmount,
			String reason, PayChannelBusinessType type) {
		Response<Boolean> result = new Response();

		try {
			Response<PayChannel> oldPayChannelRes = this.payReadService.findPayChannelByTradeNoForPaid(tradeNo);
			if (!oldPayChannelRes.isSuccess()) {
				throw new ServiceException(oldPayChannelRes.getError());
			}

			PayChannel oldPayChannel = (PayChannel) oldPayChannelRes.getResult();
			String channel = oldPayChannel.getChannel();
			PayChannel payChannel = new PayChannel();
			payChannel.setStatus(Integer.valueOf(PayChannelStatus.WAIT_HANDLE.value()));
			payChannel.setChannel(channel);
			String batchNo = RefundRequest.toBatchNo(DateTime.now().toDate(), refundTrackId);
			payChannel.setBatchNo(batchNo);
			payChannel.setTradeNo(oldPayChannel.getTradeNo());
			payChannel.setRefundOrderId(refundTrackId);
			payChannel.setPaymentCode(oldPayChannel.getPaymentCode());
			payChannel.setType(Integer.valueOf(PayChannelType.REFUND.value()));
			payChannel.setDescription("退款");
			payChannel.setOrderType(orderType);
			payChannel.setFee(refundAmount);
			payChannel.setBusinessType(Integer.valueOf(type.value()));
			payChannel.setIsCreatedDetail(Integer.valueOf(0));
			Response<PayChannel> payChannelRes = this.payWriteService.createPayChannelForRefund(payChannel);
			Preconditions.checkState(payChannelRes.isSuccess(), payChannelRes.getError());
			PayChannel existPayChannel = (PayChannel) payChannelRes.getResult();
			if (existPayChannel.getStatus().equals(Integer.valueOf(1))) {
				throw new ServiceException("can.not.duplicate.refund");
			}

			if (existPayChannel.getChannel().contains("alipay")) {
				AlipaySettingsDto alipaySettingsDto = this.paySettingsProvider.getAlipaySettings();
				String refundNotifyUrl = alipaySettingsDto.getRefundNotifyUrl();
				AlipayToken token = this.paySettingsProvider.buildAlipayToken();
				AlipayManager alipayManager = new AlipayManager();
				RedirectInfo info = alipayManager.refund(refundTrackId, reason, channel, tradeNo,
						existPayChannel.getBatchNo(), oldPayChannel.getPaymentCode(), refundAmount,
						RefundInterfaceType.NOPASSWRD.value(), refundNotifyUrl, token);
				if (info.isSuccess()) {
					return this.alipayRefund(info.getResult());
				}

				result.setError(info.getError());
			}

			if (existPayChannel.getChannel().contains("wechatpay")) {
				Response<TradePay> tradePayRes = this.payReadService.findTradePayByTradeNo(tradeNo);
				if (!tradePayRes.isSuccess()) {
					throw new ServiceException(tradePayRes.getError());
				}

				TradePay tradePay = (TradePay) tradePayRes.getResult();
				WechatpayManager wechatpayManager = new WechatpayManager();
				WxToken token = this.paySettingsProvider.buildWxToken();
				if (!this.payDebug.equals("true")) {
					WechatSettingsDto wechatSettingsDto = this.paySettingsProvider.getWechatSettings();
					String caFilePath = wechatSettingsDto.getCaFilePath();
					String certFilePath = wechatSettingsDto.getCertFilePath();
					RedirectInfo info = wechatpayManager.refund(refundTrackId, caFilePath, certFilePath, channel,
							existPayChannel.getTradeNo(), existPayChannel.getBatchNo(), oldPayChannel.getPaymentCode(),
							refundAmount, tradePay.getShouldFee(), token);
					if (!info.isSuccess()) {
						result.setError(info.getError());
						return result;
					}

					Response<Boolean> refundRes = this.wechatpayRefund(caFilePath, certFilePath, info.getResult(),
							token);
					if (!refundRes.isSuccess()) {
						return refundRes;
					}
				} else {
					RedirectInfo info = wechatpayManager.mockRefund(refundTrackId, channel,
							existPayChannel.getTradeNo(), existPayChannel.getBatchNo(), oldPayChannel.getPaymentCode(),
							refundAmount, tradePay.getShouldFee(), token);
					if (!info.isSuccess()) {
						result.setError(info.getError());
						return result;
					}

					Response<Boolean> refundRes = this.mockWechatpayRefund(info.getResult());
					if (!refundRes.isSuccess()) {
						return refundRes;
					}
				}

				return this.updateRefundSuccessForWechatRefund(existPayChannel.getBatchNo());
			}
		} catch (IllegalStateException var24) {
			log.error("refund reqeust where refundTrackId: {} fail,cause: {}", refundTrackId, var24.getMessage());
			result.setError(var24.getMessage());
		} catch (ServiceException var25) {
			log.error("refund reqeust where refundTrackId: {} fail,cause: {}", refundTrackId, var25.getMessage());
			result.setError(var25.getMessage());
		} catch (Exception var26) {
			log.error("refund reqeust where refundTrackId: {} fail,cause: {}", refundTrackId,
					Throwables.getStackTraceAsString(var26));
			result.setError("refund.reqeust.fail");
		}

		return result;
	}

	public Response alipayRefund(String url) {
		log.debug("refund url: {}", url);
		String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
		log.debug("refund result: {}", body);
		return this.convertToResponse(body);
	}

	public Response wechatpayRefund(String caFilePath, String certFilePath, String preXml, WxToken token) {
		Response<Boolean> result = new Response();

		try {
			String body = HttpsUtil.build(caFilePath, certFilePath)
					.callHttps(preXml, token.getMchId(), token.getRefundGateway()).getResContent();
			log.debug("wx order refund body: {}", body);
			WxPayRefundResponse payRefundReply = (WxPayRefundResponse) WxRequest.parse(body, WxPayRefundResponse.class);
			if (payRefundReply.isSuccess()) {
				result.setResult(Boolean.TRUE);
			} else {
				result.setError(payRefundReply.getErrorMsg());
			}
		} catch (Exception var8) {
			result.setError("wx.refund.fail");
			log.error("wx refund fail, cause: {}", Throwables.getStackTraceAsString(var8));
		}

		return result;
	}

	public Response mockWechatpayRefund(String url) {
		Response<Boolean> result = new Response();

		try {
			String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
			log.debug("wx order mock refund body: {}", body);
			WxPayRefundResponse payRefundReply = (WxPayRefundResponse) WxRequest.parse(body, WxPayRefundResponse.class);
			if (payRefundReply.isSuccess()) {
				result.setResult(Boolean.TRUE);
			} else {
				result.setError(payRefundReply.getErrorMsg());
			}
		} catch (Exception var5) {
			result.setError("wx.mock.refund.fail");
			log.error("wx mock refund fail, cause: {}", Throwables.getStackTraceAsString(var5));
		}

		return result;
	}

	protected Response convertToResponse(String body) {
		Response<Boolean> result = new Response();

		try {
			Preconditions.checkState(!Strings.isNullOrEmpty(body), "alipay.refund.fail");
			AlipaySyncResponse refundResponse = (AlipaySyncResponse) xStream.fromXML(body);
			if (refundResponse.isSuccess()) {
				result.setResult(Boolean.TRUE);
			} else {
				log.error("refund raise fail: {}", refundResponse.getError());
				result.setError(refundResponse.getError());
			}
		} catch (IllegalStateException var4) {
			log.error("alipay refund request fail,error: {}", var4.getMessage());
			result.setError("alipay.refund.fail");
		} catch (Exception var5) {
			log.error("alipay refund request fail,cause: {}", Throwables.getStackTraceAsString(var5));
			result.setError("alipay.refund.fail");
		}

		return result;
	}

	private Response updateRefundSuccessForWechatRefund(String batchNo) {
		Response<Boolean> result = new Response();

		try {
			Response<PayChannel> payChannelR = this.payReadService.findPayChannelByBatchNoForRefund(batchNo);
			if (!payChannelR.isSuccess()) {
				log.warn("fail to find pay channel by batch no {} when refund success notify, error code:{}", batchNo,
						payChannelR.getError());
				throw new ServiceException(payChannelR.getError());
			}

			PayChannel payChannel = (PayChannel) payChannelR.getResult();
			PayChannel updatePayChannel = new PayChannel();
			updatePayChannel.setId(payChannel.getId());
			updatePayChannel.setStatus(Integer.valueOf(1));
			updatePayChannel.setRefundAt(new Date());
			Response<Boolean> updateStatusRes = this.payWriteService.updatePayChannel(updatePayChannel);
			if (!updateStatusRes.isSuccess()) {
				throw new ServiceException(updateStatusRes.getError());
			}

			Map<String, Object> updateParams = Maps.newHashMap();
			updateParams.put("orderId", payChannel.getRefundOrderId());
			updateParams.put("batchNo", batchNo);
			updateParams.put("channel", payChannel.getChannel());
			updateParams.put("tradeNo", payChannel.getTradeNo());
			this.eventBus.post(new RefundedEvent(updateParams, payChannel));
			result.setResult(Boolean.TRUE);
		} catch (ServiceException var8) {
			log.error("update refund success for wechat refund fail,where batchNo: {} ,error: {}", batchNo,
					var8.getMessage());
			result.setError("update.order.refund.success.fail");
		} catch (Exception var9) {
			log.error("update refund success for wechat refund fail,where batchNo: {} ,cause: {}", batchNo,
					Throwables.getStackTraceAsString(var9));
			result.setError("update.order.refund.success.fail");
		}

		return result;
	}

	public RedirectInfo payRequest(List orderIds, Integer orderType, String channel, Integer fee, String subject,
			Long buyerId) {
		return this.payRequest(orderIds, orderType, channel, fee, subject, buyerId, (String) null);
	}

	public RedirectInfo payRequest(List orderIds, Integer orderType, String channel, Integer fee, String subject,
			Long buyerId, String returnUrl) {
		String systemNo = this.generateNo((Long) orderIds.get(0), orderType);
		TradePay tradePay = this.createPayInfo(fee, buyerId, orderIds, orderType, systemNo, subject);
		Response<PayStage> stageResponse = this.payReadService.findRecentStageBySystemNo(tradePay.getSystemNo());
		if (!stageResponse.isSuccess()) {
			log.error(
					"find recent pay stage fail,maybe current order already paid，Please use systemNo:{} to check pay_stage and pay_channel",
					tradePay.getSystemNo());
			throw new ServiceException(stageResponse.getError());
		} else {
			PayStage stage = (PayStage) stageResponse.getResult();
			PayChannel payChannel = new PayChannel();
			payChannel.setStatus(Integer.valueOf(0));
			payChannel.setType(Integer.valueOf(PayChannelType.PAID.value()));
			payChannel.setStageId(stage.getId());
			payChannel.setChannel(channel);
			payChannel.setFee(stage.getFee());
			payChannel.setOrderType(orderType);
			payChannel.setTradeNo(this.generateNo((Long) orderIds.get(0), orderType));
			payChannel.setCreatedAt(new Date());
			payChannel.setUpdatedAt(new Date());
			payChannel.setIsCreatedDetail(Integer.valueOf(0));
			payChannel.setBusinessType(Integer.valueOf(PayChannelBusinessType.PAID.value()));
			Response<PayChannel> payChannelRes = this.payWriteService.createPayChannel(payChannel);
			if (!payChannelRes.isSuccess()) {
				throw new ServiceException(payChannelRes.getError());
			} else {
				PayChannel existPaychannel = (PayChannel) payChannelRes.getResult();
				if (existPaychannel.getStatus().equals(Integer.valueOf(1))) {
					throw new ServiceException("can.not.duplicate.pay");
				} else {
					return channel.contains("alipay")
											? this.getAlipayRedirectInfo(channel, subject, stage.getContent(),
													existPaychannel.getTradeNo(), systemNo, payChannel.getFee(),
													returnUrl)
											: (channel.contains("wechatpay")
																							? this.getWechatpayRedirectInfo(
																									channel, subject,
																									stage.getContent(),
																									existPaychannel
																											.getTradeNo(),
																									fee, buyerId)
																							: null);
				}
			}
		}
	}

	private RedirectInfo getAlipayRedirectInfo(String channel, String subject, String content, String tradNo,
			String systemNo, Integer fee, String returnUrl) {
		AlipayManager manager = new AlipayManager();
		Date expiredAt = (new DateTime()).plusDays(3).minusHours(3).toDate();
		AlipaySettingsDto alipaySettingsDto = this.paySettingsProvider.getAlipaySettings();
		String notifyUrl = alipaySettingsDto.getNotifyUrl();
		if (Strings.isNullOrEmpty(returnUrl)) {
			returnUrl = alipaySettingsDto.getReturnUrl();
		}

		AlipayToken token = this.paySettingsProvider.buildAlipayToken();
		RedirectInfo info = manager.pay(channel, subject, content, tradNo, systemNo, expiredAt, fee, notifyUrl,
				returnUrl, token);
		if (!info.isSuccess()) {
			throw new ServiceException(info.getError());
		} else {
			return info;
		}
	}

	private RedirectInfo getWechatpayRedirectInfo(String channel, String subject, String content, String tradeNo,
			Integer fee, Long buyerId) {
		RedirectInfo redirect = new RedirectInfo();
		WechatpayManager wechatpayManager = new WechatpayManager();
		WechatSettingsDto wechatSettingsDto = this.paySettingsProvider.getWechatSettings();
		String notifyUrl = wechatSettingsDto.getNotifyUrl();
		WxToken token = this.paySettingsProvider.buildWxToken();
		if (!this.payDebug.equals("true")) {
			RedirectInfo prePayInfo = wechatpayManager.prePay(channel, subject, content, tradeNo, fee, notifyUrl,
					buyerId, token);
			if (!prePayInfo.isSuccess()) {
				log.error("generate pre pay info fail where channel:{} tradeNo:{},error:{}",
						new Object[] { channel, tradeNo, prePayInfo.getError() });
				throw new ServiceException(prePayInfo.getError());
			} else {
				Response<WxPrePayResponse> response = this.wxPrePay(prePayInfo.getResult(), token);
				if (!response.isSuccess()) {
					log.error("pre pay post fail where channel:{} tradeNo:{},error:{}",
							new Object[] { channel, tradeNo, response.getError() });
					throw new ServiceException(response.getError());
				} else {
					RedirectInfo payInfo = wechatpayManager.pay(channel, tradeNo,
							(WxPrePayResponse) response.getResult(), token);
					if (!payInfo.isSuccess()) {
						log.error("generate pay info fail where channel:{} tradeNo:{},error:{}",
								new Object[] { channel, tradeNo, payInfo.getError() });
						throw new ServiceException(payInfo.getError());
					} else {
						if (channel.endsWith("jsapi")) {
							redirect.setIsRedirectNow(Boolean.FALSE);
							redirect.setChannel(channel);
							redirect.setResult(payInfo.getResult());
						} else {
							HashMap payContext = (HashMap) JsonMapper.nonEmptyMapper().fromJson(payInfo.getResult(),
									HashMap.class);
							File file = (new QrHelper())
									.getQrCode(((WxPrePayResponse) response.getResult()).getCodeUrl());
							String qrcodePicUrl = this.imageServer
									.write("/" + buyerId.toString() + "/" + FileUtil.newFileName(file.getName()), file);
							String payView = "http://" + ThreadVars.getHost() + "/pay/mobile-pay";
							payView = payView + "?qrcodeUrl=" + wechatSettingsDto.getImageBaseUrl() + qrcodePicUrl
									+ "&tradeNo=" + payContext.get("tradeNo");
							redirect.setIsRedirectNow(Boolean.TRUE);
							redirect.setChannel(channel);
							redirect.setResult(payView);
						}

						return redirect;
					}
				}
			}
		} else {
			RedirectInfo prePayInfo = wechatpayManager.mockPay(channel, subject, content, tradeNo, fee, notifyUrl,
					buyerId, token);
			return prePayInfo;
		}
	}

	public Response wxPrePay(String preXml, WxToken token) {
		Response<WxPrePayResponse> result = new Response();
		log.debug("wx prepay url: {}", token.getGateway());
		log.debug("wx prepay pre xml: {}", preXml);
		String body = HttpRequest.post(token.getGateway()).send(preXml).trustAllCerts().trustAllHosts().body();
		log.debug("wx prepay body: {}", body);
		if (WxRequest.verify(body, token)) {
			WxPrePayResponse wxPrePayResponse = (WxPrePayResponse) WxRequest.parse(body, WxPrePayResponse.class);
			result.setResult(wxPrePayResponse);
		}

		return result;
	}

	private void checkParam(Map map) {
		if (!map.containsKey("orderId")) {
			throw new ServiceException("prepay.action.param.order.ids.invalid");
		} else if (!map.containsKey("channel")) {
			throw new ServiceException("prepay.action.param.channel.invalid");
		}
	}

	private TradePay createPayInfo(Integer fee, Long buyerId, List ids, Integer orderType, String systemNo,
			String subject) {
		String orderIds = Joiners.COMMA.join(ids);
		Response<TradePay> existTradePayRes = this.payReadService.checkNeedCreateTradePay(orderIds,
				ids.size() > 1 ? Boolean.TRUE : Boolean.FALSE, orderType);
		if (!existTradePayRes.isSuccess()) {
			throw new ServiceException(existTradePayRes.getError());
		} else if (Arguments.notNull(existTradePayRes.getResult())) {
			return (TradePay) existTradePayRes.getResult();
		} else {
			List<PayStageInfo> stageInfos = Lists.newArrayList();
			PayStageInfo info = new PayStageInfo();
			info.setCurrentStage(Integer.valueOf(1));
			info.setFee(fee);
			stageInfos.add(info);
			if (Arguments.isNullOrEmpty(stageInfos)) {
				throw new ServiceException("stage.info.invalid");
			} else {
				TradePay tradePay = new TradePay();
				tradePay.setBuyerId(buyerId);
				tradePay.setMergePaid(ids.size() > 1 ? Boolean.TRUE : Boolean.FALSE);
				tradePay.setShouldFee(this.getMultiOrderFee(stageInfos));
				tradePay.setAlreadyFee(Integer.valueOf(0));
				tradePay.setOrderId(ids.size() > 1 ? null : (Long) ids.get(0));
				tradePay.setOrderIds(orderIds);
				tradePay.setOrderType(orderType);
				tradePay.setPaidStatus(Integer.valueOf(0));
				tradePay.setCurrentStage(Integer.valueOf(0));
				tradePay.setPaidType(Integer.valueOf(1));
				tradePay.setStage(Integer.valueOf(stageInfos.size()));
				tradePay.setSubject(subject);
				tradePay.setSystemNo(systemNo);
				tradePay.setCreatedAt(new Date());
				tradePay.setUpdatedAt(new Date());
				Response<Boolean> tradePayRes = this.payWriteService.createTradePayWithStage(tradePay, stageInfos,
						(Date) null);
				if (!tradePayRes.isSuccess()) {
					throw new ServiceException(tradePayRes.getError());
				} else {
					return tradePay;
				}
			}
		}
	}

	private Integer getMultiOrderFee(List<PayStageInfo> infos) {
		int total = 0;

		for (PayStageInfo info : infos) {
			total += info.getFee().intValue();
		}

		return Integer.valueOf(total);
	}

	private String generateNo(Long orderId, Integer orderType) {
		String prefix = DFT.print(DateTime.now());
		String suffix = orderId.toString();
		Integer orderLenght = Integer.valueOf(suffix.length());
		String random = GenerateRandom.rand(15 - orderLenght.intValue());
		return prefix + orderType + random + suffix;
	}

	static {
		xStream.autodetectAnnotations(true);
		xStream.processAnnotations(AlipaySyncResponse.class);
	}
}

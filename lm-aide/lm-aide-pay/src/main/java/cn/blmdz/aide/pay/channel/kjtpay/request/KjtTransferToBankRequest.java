package cn.blmdz.aide.pay.channel.kjtpay.request;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.base.Splitter.MapSplitter;
import com.google.common.base.Strings;
import com.google.common.collect.Maps;

import cn.blmdz.aide.pay.channel.kjtpay.dto.TransferToBankInfo;
import cn.blmdz.aide.pay.channel.kjtpay.util.CoreTransfer;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.NumberUtils;

public class KjtTransferToBankRequest extends RequestTransfer {
	private static final Logger log = LoggerFactory.getLogger(KjtTransferToBankRequest.class);
	private static final String SEPARATE = "~";
	private static final String ROW_SEPARATE = "$";
	private static final String ENCRYPT_CHARSET = "UTF-8";

	private KjtTransferToBankRequest(KjtTransferToken kjtTransferToken) {
		super(kjtTransferToken);
		this.params.put("service", "create_batch_transfer_to_card");
		this.params.put("memo", "快捷通到银行卡批量转账");
		this.params.put("identity_type", "1");
	}

	public static KjtTransferToBankRequest build(KjtTransferToken kjtTransferToken) {
		return new KjtTransferToBankRequest(kjtTransferToken);
	}

	public KjtTransferToBankRequest batchNo(String requestNo) {
		Preconditions.checkArgument(Arguments.notEmpty(requestNo), "kjt.pay.outer.batch.no.empty");
		this.params.put("batch_no", requestNo);
		return this;
	}

	public KjtTransferToBankRequest transferNum(Integer num) {
		Preconditions.checkArgument(Arguments.notNull(num), "kjt.pay.transfer.num.null");
		this.params.put("transfer_num", num.toString());
		return this;
	}

	public KjtTransferToBankRequest transferAmount(String amount) {
		Preconditions.checkArgument(!Strings.isNullOrEmpty(amount), "kjt.pay.transfer.amount.null");
		this.params.put("transfer_amount", amount);
		return this;
	}

	public KjtTransferToBankRequest transferList(List<TransferToBankInfo> transferList) {
		Preconditions.checkArgument(!Arguments.isNullOrEmpty(transferList), "kjt.pay.transfer.list.null");
		StringBuilder builder = new StringBuilder();

		for (int i = 0; i < transferList.size(); ++i) {
			TransferToBankInfo info = (TransferToBankInfo) transferList.get(i);
			builder.append(info.getOutTradeNum()).append(SEPARATE).append(info.getAccountName()).append(SEPARATE)
					.append(info.getAccountNo()).append(SEPARATE).append(info.getBankNo()).append(SEPARATE)
					.append(info.getBankName()).append(SEPARATE).append(info.getBankBranchName()).append(SEPARATE)
					.append(info.getBankBranchNo()).append(SEPARATE).append(info.getBankProvice()).append(SEPARATE)
					.append(info.getBankCity()).append(SEPARATE).append(info.getAccountType()).append(SEPARATE)
					.append(NumberUtils.formatPrice(Long.valueOf(info.getAmount()))).append(SEPARATE).append(info.getMemo());
			if (i != transferList.size() - 1) {
				builder.append(ROW_SEPARATE);
			}
		}

		String content = builder.toString();
		if (Arguments.notEmpty(content)) {
			String encryption = CoreTransfer.encryptData(content, ENCRYPT_CHARSET);
			this.params.put("transfer_list", encryption);
		}

		return this;
	}

	public KjtTransferToBankRequest operatorId(String operatorId) {
		if (Arguments.notEmpty(operatorId)) {
			this.params.put("operator_id", operatorId);
		}

		return this;
	}

	public KjtTransferToBankRequest identityNo(String identityNo) {
		Preconditions.checkArgument(Arguments.notEmpty(identityNo), "kjt.pay.identity.no.empty");
		this.params.put("identity_no", identityNo);
		return this;
	}

	public KjtTransferToBankRequest notify(String notify) {
		if (Arguments.notNull(notify)) {
			this.params.put("notify_url", notify);
		}

		return this;
	}

	public KjtTransferToBankRequest setSign(String sign) {
		Preconditions.checkArgument(Arguments.notNull(sign), "kjt.pay.sign.empty");
		this.params.put("sign", sign);
		return this;
	}

	public KjtTransferToBankRequest setSignType(String type) {
		Preconditions.checkArgument(Arguments.notNull(type), "kjt.pay.sign_type.empty");
		this.params.put("sign_type", type);
		return this;
	}

	public Map<String, String> transfer() throws Exception {
		Map<String, String> transferMap = CoreTransfer.buildRequestPara(super.params, "ITRUSSRV", "", ENCRYPT_CHARSET);
		log.info("kjtpay transfer url: {}, params : {}", this.kjtTransferToken.getGateway(), transferMap);
		String body = HttpRequest.post(this.kjtTransferToken.getGateway()).form(transferMap).connectTimeout(10000)
				.readTimeout(10000).body();
		log.info("kjtpay transfer result: {}", body);
		MapSplitter splitter = Splitter.on("&").withKeyValueSeparator("=");
		Map<String, String> map = splitter.split(body);
		if (((String) map.get("is_success")).equals("T")) {
			return transferMap;
		} else {
			log.info("error_message is: {}", map.get("error_message"));
			return Maps.newHashMap();
		}
	}
}

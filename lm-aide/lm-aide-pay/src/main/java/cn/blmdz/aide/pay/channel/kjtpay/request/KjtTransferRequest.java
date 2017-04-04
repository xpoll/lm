package cn.blmdz.aide.pay.channel.kjtpay.request;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.base.Splitter.MapSplitter;
import com.google.common.base.Strings;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.util.Arguments;

public class KjtTransferRequest extends RequestTransfer {
	private static final Logger log = LoggerFactory.getLogger(KjtTransferRequest.class);

	private KjtTransferRequest(KjtTransferToken kjtTransferToken) {
		super(kjtTransferToken);
		this.params.put("service", "create_batch_transfer_to_account");
		this.params.put("memo", "");
		this.params.put("identity_type", "1");
	}

	public static KjtTransferRequest build(KjtTransferToken kjtTransferToken) {
		return new KjtTransferRequest(kjtTransferToken);
	}

	public KjtTransferRequest memo(String memo) {
		if (Arguments.notNull(memo)) {
			this.params.put("memo", memo);
		}

		return this;
	}

	public KjtTransferRequest returnUrl(String forward) {
		if (Arguments.notNull(forward)) {
			this.params.put("return_url", forward);
		}

		return this;
	}

	public KjtTransferRequest notify(String notify) {
		if (Arguments.notNull(notify)) {
			this.params.put("notify_url", notify);
		}

		return this;
	}

	public KjtTransferRequest transferList(String content) {
		if (Arguments.notEmpty(content)) {
			this.params.put("transfer_list", content);
		}

		return this;
	}

	public KjtTransferRequest operatorId(String operatorId) {
		if (Arguments.notEmpty(operatorId)) {
			this.params.put("operator_id", operatorId);
		}

		return this;
	}

	public KjtTransferRequest batchNo(String requestNo) {
		Preconditions.checkArgument(Arguments.notEmpty(requestNo), "kjt.pay.outer.batch.no.empty");
		this.params.put("batch_no", requestNo);
		return this;
	}

	public KjtTransferRequest identityNo(String identityNo) {
		Preconditions.checkArgument(Arguments.notEmpty(identityNo), "kjt.pay.identity.no.empty");
		this.params.put("identity_no", identityNo);
		return this;
	}

	public KjtTransferRequest transferNum(Integer num) {
		Preconditions.checkArgument(Arguments.notNull(num), "kjt.pay.transfer.num.null");
		this.params.put("transfer_num", num.toString());
		return this;
	}

	public KjtTransferRequest transferAmount(String amount) {
		Preconditions.checkArgument(!Strings.isNullOrEmpty(amount), "kjt.pay.transfer.amount.null");
		this.params.put("transfer_amount", amount);
		return this;
	}

	public KjtTransferRequest setSign(String sign) {
		Preconditions.checkArgument(Arguments.notNull(sign), "kjt.pay.sign.empty");
		this.params.put("sign", sign);
		return this;
	}

	public KjtTransferRequest setSignType(String type) {
		Preconditions.checkArgument(Arguments.notNull(type), "kjt.pay.sign_type.empty");
		this.params.put("sign_type", type);
		return this;
	}

	public Map<String, String> transfer() {
		String url = super.url();
		log.info("kjtpay transfer url: {}", url);
		String body = HttpRequest.get(url).connectTimeout('썐').readTimeout('썐').body();
		log.info("kjtpay transfer result: {}", body);
		MapSplitter splitter = Splitter.on("&").withKeyValueSeparator("=");
		Map<String, String> map = splitter.split(body);
		if (((String) map.get("is_success")).equals("T")) {
			return map;
		} else {
			log.info("error_message is: {}", map.get("error_message"));
			return Maps.newHashMap();
		}
	}
}

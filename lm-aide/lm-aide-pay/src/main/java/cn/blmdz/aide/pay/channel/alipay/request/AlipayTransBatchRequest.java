package cn.blmdz.aide.pay.channel.alipay.request;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;

import cn.blmdz.aide.pay.channel.alipay.dto.AlipayTransBatchData;
import cn.blmdz.home.common.util.Arguments;

public class AlipayTransBatchRequest extends Request {
	private static final Logger log = LoggerFactory.getLogger(AlipayTransBatchRequest.class);
	private static final Joiner DETAIL_JOINER = Joiner.on("^").skipNulls();
	private static final Joiner TRANS_JOINER = Joiner.on("|").skipNulls();
	private static final DateTimeFormatter DFT_BATCH = DateTimeFormat.forPattern("yyyyMMdd");

	protected AlipayTransBatchRequest(AlipayToken alipayToken) {
		super(alipayToken);
		this.params.put("service", "batch_trans_notify");
		this.params.put("email", alipayToken.getAccount());
	}

	public static AlipayTransBatchRequest build(AlipayToken alipayToken) {
		return new AlipayTransBatchRequest(alipayToken);
	}

	public AlipayTransBatchRequest notify(CallBack notify) {
		if (Arguments.notNull(notify)) {
			this.params.put("notify_url", notify.getUrl());
		}

		return this;
	}

	public AlipayTransBatchRequest accountName(String accountName) {
		Preconditions.checkArgument(Arguments.notEmpty(accountName), "alipay.trans.batch.account.name.empty");
		if (Arguments.notEmpty(accountName)) {
			this.params.put("account_name", accountName);
		}

		return this;
	}

	public AlipayTransBatchRequest detailData(List<AlipayTransBatchData> trans) {
		Preconditions.checkArgument(Arguments.notEmpty(trans), "alipay.trans.batch.detail.empty");
		Preconditions.checkArgument(trans.size() <= 1000, "alipay.trans.batch.detail.too.much");
		String detailData = this.getTransDetail(trans);
		Preconditions.checkArgument(Arguments.notEmpty(detailData), "alipay.trans.batch.detail.data.empty");
		if (Arguments.notEmpty(detailData)) {
			this.params.put("detail_data", detailData);
			this.params.put("batch_num", trans.size() + "");
		}

		return this;
	}

	public AlipayTransBatchRequest batchNo(String batchNo) {
		Preconditions.checkArgument(Arguments.notEmpty(batchNo), "alipay.trans.batch.no.empty");
		Preconditions.checkArgument(batchNo.length() >= 11 && batchNo.length() <= 32, "alipay.trans.batch.no.illegal");
		if (Arguments.notEmpty(batchNo)) {
			this.params.put("batch_no", batchNo);
		}

		return this;
	}

	public AlipayTransBatchRequest batchFee(Integer batchFee) {
		Preconditions.checkArgument(Arguments.notNull(batchFee), "alipay.trans.batch.fee.empty");
		if (Arguments.notNull(batchFee)) {
			this.params.put("batch_fee", DECIMAL_FORMAT.format((double) batchFee.intValue() / 100.0D));
		}

		return this;
	}

	public AlipayTransBatchRequest payDate(Date payDate) {
		if (Arguments.notNull(payDate)) {
			this.params.put("pay_date", DateTime.now().toString(DFT_BATCH));
		}

		return this;
	}

	public AlipayTransBatchRequest buyerAccountName(String buyerAccountName) {
		if (Arguments.notEmpty(buyerAccountName)) {
			this.params.put("buyer_account_name", buyerAccountName);
		}

		return this;
	}

	public AlipayTransBatchRequest extendParam(Map<String, String> extendParam) {
		if (Arguments.notNull(extendParam) && !extendParam.isEmpty()) {
			List<String> extendParamList = Lists.newArrayList();

			for (Entry<String, String> entry : extendParam.entrySet()) {
				extendParamList.add(DETAIL_JOINER.join(entry.getKey(), entry.getValue(), new Object[0]));
			}

			this.params.put("extend_param", TRANS_JOINER.join(extendParamList));
		}

		return this;
	}

	public Boolean trans() {
		String url = super.url();
		log.debug("trans url: {}", url);
		String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
		log.debug("trans result: {}", body);
		return this.convertToResponse(body);
	}

	private String getTransDetail(List<AlipayTransBatchData> trans) {
		List<String> transDetails = Lists.newArrayListWithCapacity(trans.size());

		for (AlipayTransBatchData tran : trans) {
			String batchNo = tran.getBatchNo();
			String account = tran.getAccount();
			String accountName = tran.getAccountName();
			String remark = tran.getRemark();
			Integer fee = tran.getFee();
			String transString = DETAIL_JOINER.join(batchNo, account,
					new Object[] { accountName, DECIMAL_FORMAT.format((double) fee.intValue() / 100.0D), remark });
			transDetails.add(transString);
		}

		return TRANS_JOINER.join(transDetails);
	}

	public void sign() {
		try {
			this.params.remove("seller_email");
			super.sign();
		} catch (Exception var2) {
			throw new RuntimeException(var2);
		}
	}
}

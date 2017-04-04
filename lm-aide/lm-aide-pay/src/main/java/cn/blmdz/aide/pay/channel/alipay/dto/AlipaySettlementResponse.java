package cn.blmdz.aide.pay.channel.alipay.dto;

import java.util.List;

import com.google.common.base.Objects;
import com.thoughtworks.xstream.annotations.XStreamAlias;

import cn.blmdz.aide.pay.channel.alipay.dto.settlement.AlipaySettlementParam;
import cn.blmdz.aide.pay.channel.alipay.dto.settlement.AlipaySettlementResult;
import cn.blmdz.home.common.util.Arguments;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
@XStreamAlias("alipay")
public class AlipaySettlementResponse {
	@XStreamAlias("is_success")
	private String success = "F";
	@XStreamAlias("request")
	private List<AlipaySettlementParam> request;
	@XStreamAlias("response")
	private AlipaySettlementResult result;
	@XStreamAlias("sign")
	private String sign;
	@XStreamAlias("sign_type")
	private String signType;

	public Boolean isSuccess() {
		return Boolean.valueOf(Arguments.notEmpty(this.success) && Objects.equal(this.success, "T"));
	}

	public Boolean hasNextPage() {
		return Boolean.valueOf(Arguments.notNull(this.result) && Arguments.notNull(this.result.getPaging())
				&& Arguments.notEmpty(this.result.getPaging().getHasNextPage())
				&& Arguments.equalWith(this.result.getPaging().getHasNextPage(), "T"));
	}
}

package cn.blmdz.aide.pay.channel.alipay.dto.settlement;

import com.thoughtworks.xstream.annotations.XStreamAlias;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class AlipaySettlementResult {
	@XStreamAlias("account_page_query_result")
	private AlipaySettlementPaging paging;
}

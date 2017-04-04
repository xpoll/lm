package cn.blmdz.aide.pay.channel.alipay.dto.settlement;

import com.thoughtworks.xstream.annotations.XStreamAlias;

import lombok.Data;

import java.util.List;

@Data
public class AlipaySettlementPaging {
	@XStreamAlias("account_log_list")
	private List<?> accountLogList;
	@XStreamAlias("has_next_page")
	private String hasNextPage = "F";
	@XStreamAlias("page_no")
	private String pageNo;
	@XStreamAlias("page_size")
	private String pageSize;
}

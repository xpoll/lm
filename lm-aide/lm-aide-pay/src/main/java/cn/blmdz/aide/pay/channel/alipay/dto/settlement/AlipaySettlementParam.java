package cn.blmdz.aide.pay.channel.alipay.dto.settlement;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamConverter;
import com.thoughtworks.xstream.converters.extended.ToAttributedValueConverter;

import lombok.AllArgsConstructor;
import lombok.ToString;

@XStreamAlias("param")
@XStreamConverter(value = ToAttributedValueConverter.class, strings = { "value" })
@AllArgsConstructor
@ToString
public class AlipaySettlementParam {
	@XStreamAlias("name")
	private String name;
	private String value;
}

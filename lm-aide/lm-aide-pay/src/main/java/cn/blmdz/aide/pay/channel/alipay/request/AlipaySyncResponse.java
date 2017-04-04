package cn.blmdz.aide.pay.channel.alipay.request;

import com.google.common.base.Objects;
import com.thoughtworks.xstream.annotations.XStreamAlias;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@XStreamAlias("alipay")
public class AlipaySyncResponse {
	@XStreamAlias("is_success")
	private String success;
	@XStreamAlias("error")
	private String error;

	public boolean isSuccess() {
		return Objects.equal(this.success, "T");
	}
}

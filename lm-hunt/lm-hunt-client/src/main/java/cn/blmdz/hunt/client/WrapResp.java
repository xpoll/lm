package cn.blmdz.hunt.client;

import java.io.Serializable;

import cn.blmdz.hunt.common.InnerCookie;
import lombok.Getter;
import lombok.Setter;

public class WrapResp implements Serializable {
	private static final long serialVersionUID = 1025854443836742035L;

	@Getter
	@Setter
	private Object result;
	@Getter
	@Setter
	private InnerCookie cookie;
}
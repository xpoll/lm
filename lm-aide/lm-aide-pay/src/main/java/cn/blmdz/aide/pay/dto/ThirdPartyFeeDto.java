package cn.blmdz.aide.pay.dto;

import java.io.Serializable;

import lombok.Data;

@Data
public class ThirdPartyFeeDto implements Serializable {
	private static final long serialVersionUID = 1L;
	private Long fee;
	private Long thirdPartyFee;
	private String thirdPartyRate;
	private String channel;
}

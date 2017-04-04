package cn.blmdz.aide.sms.impl.alibaba;

import java.io.Serializable;

import lombok.Data;

@Data
public class AliSmsSendSuccessResult implements Serializable {
	private static final long serialVersionUID = 1L;
	private AliSmsSendSuccessResult.Response alibaba_aliqin_fc_sms_num_send_response;

	@Data
	public static class Response implements Serializable {
		private static final long serialVersionUID = 1L;
		private AliSmsSendSuccessResult.Response.Result result;
		private String request_id;

		@Data
		public static class Result implements Serializable {
			private static final long serialVersionUID = 1L;
			private String model;
			private Boolean success;

		}
	}
}

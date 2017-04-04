package cn.blmdz.aide.apppush.impl.umeng;

import java.io.Serializable;

import lombok.Data;

@Data
public class UmengMessage implements Serializable {
	private static final long serialVersionUID = 1L;
	private String appkey;
	private String timestamp = Integer.toString((int) (System.currentTimeMillis() / 1000L));
	private String type = "unicast";
	private String device_tokens;
	private String alias_type;
	private String alias;
	private String file_id;
	private Filter filter = new Filter();
	private Policy policy = new Policy();
	private String production_mode = Boolean.FALSE.toString();
	private String description;
	private String thirdparty_id;

	@Data
	public static class Filter implements Serializable {
		private static final long serialVersionUID = 1L;
		private String where;
	}

	@Data
	public static class Policy implements Serializable {
		private static final long serialVersionUID = 1L;
		private String start_time;
		private String expire_time;
		private String max_send_num;
		private String out_biz_no;
	}
}

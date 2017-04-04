package cn.blmdz.aide.apppush.impl.umeng;

import java.io.Serializable;
import java.util.Map;

import com.google.common.collect.Maps;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper=false)
public class UmengMessageAndroid extends UmengMessage {
	private static final long serialVersionUID = 1L;
	private AndroidPayload payload = new AndroidPayload();
	
	@Data
	public static class AndroidPayload implements Serializable {
		private static final long serialVersionUID = 1L;
		private String display_type = "notification";
		private Body body = new Body();
		private Map<?, ?> extra = Maps.newHashMap();
		
		@Data
		public static class Body implements Serializable {
			private static final long serialVersionUID = 1L;
			private String ticker;
			private String title;
			private String text;
			private String icon;
			private String largeIcon;
			private String img;
			private String sound;
			private String builder_id;
			private String play_vibrate;
			private String play_lights;
			private String play_sound;
			private String after_open = "go_app";
			private String url;
			private String activity;
			private String custom;
		}
	}
}

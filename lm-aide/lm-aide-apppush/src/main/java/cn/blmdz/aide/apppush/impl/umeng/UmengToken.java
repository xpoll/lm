package cn.blmdz.aide.apppush.impl.umeng;

import java.io.Serializable;

import lombok.Data;

@Data
public class UmengToken implements Serializable {
	private static final long serialVersionUID = 1L;
	private String appKey;
	private String appMasterSecret;
	private String sendUrl = "http://msg.umeng.com/api/send";
}

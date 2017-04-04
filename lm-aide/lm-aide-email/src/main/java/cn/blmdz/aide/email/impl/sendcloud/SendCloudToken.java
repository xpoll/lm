package cn.blmdz.aide.email.impl.sendcloud;

import java.io.Serializable;

import lombok.Data;

@Data
public class SendCloudToken implements Serializable {
	private static final long serialVersionUID = 1;
	private String user;
	private String triggerUser;
	private String batchUser;
	private String key;
	private String gateway;
}

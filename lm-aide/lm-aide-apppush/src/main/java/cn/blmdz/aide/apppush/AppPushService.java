package cn.blmdz.aide.apppush;

public interface AppPushService {
	String send(String deviceType, String deviceTokens, String title, String content) throws AppPushException;
}

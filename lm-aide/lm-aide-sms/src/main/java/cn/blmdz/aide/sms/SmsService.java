package cn.blmdz.aide.sms;

public interface SmsService {
	String send(String from, String toes, String message, String extra) throws SmsException;

	String send(String from, String toes, String message) throws SmsException;

	Integer available();
}

package cn.blmdz.aide.email;

public interface EmailService {
   String send(String subject, String content, String toes, String attachments) throws EmailException;
}

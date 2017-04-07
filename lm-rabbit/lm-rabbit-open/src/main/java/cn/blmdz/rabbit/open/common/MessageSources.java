package cn.blmdz.rabbit.open.common;

import java.util.Locale;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;

/**
 * Mail: xiao@terminus.io <br>
 * Date: 2016-03-21 6:42 PM  <br>
 * Author: xiao
 */
public class MessageSources implements OPMessageSource {

    @Autowired
    private MessageSource messageSource;

    public String get(String code, Object... args) {
        if (messageSource == null) {
            return code;
        }
        return messageSource.getMessage(code, args, code, Locale.SIMPLIFIED_CHINESE);
    }
}
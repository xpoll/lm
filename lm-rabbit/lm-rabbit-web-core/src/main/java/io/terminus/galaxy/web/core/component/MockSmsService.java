package io.terminus.galaxy.web.core.component;

import io.terminus.lib.sms.SmsException;
import io.terminus.lib.sms.SmsService;
import io.terminus.parana.common.util.MapUtil;
import lombok.extern.slf4j.Slf4j;

/**
 * 这个是mock用的service, 主要功能是打日志.
 * 用于在项目启动时, 为了使注册等流程不会因为网关实现而阻塞.
 *
 * DATE: 16/6/13 上午9:28 <br>
 * MAIL: zhanghecheng@terminus.io <br>
 * AUTHOR: zhanghecheng
 */
@Slf4j
public class MockSmsService implements SmsService {

    /**
     * 发送单条信息, 或者群发消息
     * @param from    发送方
     * @param toes    接收方手机号码, json列表["xxx","xxx"], 单个时可直接为"xxxxx"
     * @param message 消息体
     * @param extra   额外的消息参数, json格式, 可为空
     * @return  成功则返回额外的信息, 错误则抛出异常SmsException
     */
    @Override
    public String send(String from, String toes, String message, String extra) throws SmsException{
        log.info(MapUtil.from().of("from", from, "toes", toes, "message", message, "extra", extra).toString());
        return "true";
    }

    /**
     * 发送单条信息, 或者群发消息
     * @param from    发送方
     * @param toes    接收方手机号码, json列表["xxx","xxx"], 单个时可直接为"xxxxx"
     * @param message 消息体
     * @return  成功则返回额外的信息, 错误则抛出异常SmsException
     */
    @Override
    public String send(String from, String toes, String message) throws SmsException{
        log.info(MapUtil.from().of("from", from, "toes", toes, "message", message).toString());
        return "true";
    }

    /**
     * 查询剩余短信条数
     * @return 剩余短信条数
     */
    @Override
    public Integer available(){
        return 100000;
    }
}

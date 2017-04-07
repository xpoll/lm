/*
 * Copyright (c) 2014 杭州端点网络科技有限公司
 */

package cn.blmdz.rabbit.pay.enums;

/**
 * <pre>
 *   功能描述: 支付渠道
 *      1. "alipay", "支付宝支付"
 *      2. "wapalipay", "wap支付宝支付"
 *      3. "ccbapppay", "建行客户端支付"
 *      4. "ccbpay", "建行支付"
 *      5. "wechatpay", "微信支付"
 *      6. "kjtpay", "快捷通支付"
 * </pre>
 *
 * @author wanggen on 2014-12-18.
 */
public enum PayChannelEnum {

    /**
     * 支付宝支付-pc
     */
    Alipay("alipay", "支付宝PC支付"),
    /**
     * 支付宝支付-pc
     */
    MockAlipay("mock-alipay", "mock支付宝PC支付"),
    /**
     * wap支付宝支付-wap
     */
    Wapalipay("wapalipay", "支付宝移动终端支付"),
    /**
     * 建行二维码支付-code
     */
    Ccbapppay("ccbapppay", "建行客户端支付"),
    /**
     * 建行网页支付
     */
    Ccbpay("ccbpay", "建行PC支付"),
    /**
     * 微信支付-native
     */
    Wechatpay("wechatpay", "微信二维码支付"),
    /**
     * 微信支付-jsapi
     */
    Wechatpay_jsapi("wechatpay-jsapi", "微信客户端控件支付"),

    /**
     * mock微信支付-jsapi
     */
    MockWechatpay_jsapi("mock-wechatpay-jsapi", "mock微信客户端控件支付"),

    /**
     * 快捷通支付
     */
    Kjtpay("kjtpay", "快捷通支付"),

    /**
     * 东南亚mol第三方支付
     */
    MOLpay("molpay", "mol支付"),

    /**
     * 东南亚mol第三方支付
     */
    MockMOLpay("mock-molpay", "mock-mol支付"),

    /**
     * 银联支付
     */
    Unionpay("unionpay", "银联pc支付");


    public static PayChannelEnum from(String channel) {
        for (PayChannelEnum ch : PayChannelEnum.values()) {
            if (ch.name.equals(channel)) return ch;
        }
        throw new IllegalArgumentException("Illegal pay channel:[" + channel + "]");
    }

    public final String name;
    public final String description;

    private PayChannelEnum(String value, String description) {
        this.name = value;
        this.description = description;
    }

}
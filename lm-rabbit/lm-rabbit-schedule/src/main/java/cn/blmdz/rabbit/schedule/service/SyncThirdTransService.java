package cn.blmdz.rabbit.schedule.service;

import static com.google.common.base.Preconditions.checkState;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;

import cn.blmdz.aide.pay.channel.alipay.config.AlipaySettings;
import cn.blmdz.aide.pay.channel.alipay.dto.AlipaySettlementResponse;
import cn.blmdz.aide.pay.channel.alipay.dto.RedirectInfo;
import cn.blmdz.aide.pay.channel.alipay.dto.settlement.AlipaySettlementDto;
import cn.blmdz.aide.pay.channel.alipay.manager.AlipayManager;
import cn.blmdz.aide.pay.channel.alipay.request.AlipayToken;
import cn.blmdz.aide.pay.channel.alipay.request.PageQueryRequest;
import cn.blmdz.aide.pay.channel.wechatpay.config.WechatSettings;
import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPayBillDto;
import cn.blmdz.aide.pay.channel.wechatpay.dto.WxPayBillResponse;
import cn.blmdz.aide.pay.channel.wechatpay.manager.WechatpayManager;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxDownloadBillRequest;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxRequest;
import cn.blmdz.aide.pay.channel.wechatpay.request.WxToken;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.config.ConfigCenter;
import cn.blmdz.wolf.pay.model.AlipayTrans;
import cn.blmdz.wolf.pay.model.KjtpayTrans;
import cn.blmdz.wolf.pay.model.UnionPayTrans;
import cn.blmdz.wolf.pay.model.WechatpayTrans;
import cn.blmdz.wolf.pay.service.PayWriteService;
import lombok.extern.slf4j.Slf4j;

/**
 * 同步第三方账务
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/21/16
 * Time: 5:51 PM
 */
@Component @Slf4j
public class SyncThirdTransService {

    @Autowired
    private ConfigCenter configCenter;

    @Autowired
    private PayWriteService payWriteService;

    private DateTimeFormatter STD_FMT = DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss");
    private final static DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyy");
    private final DateTimeFormatter DT = DateTimeFormat.forPattern("MMdd");
    private final static DateTimeFormatter DFT2 = DateTimeFormat.forPattern("yyyyMMddHHmmss");
    private static final DateTimeFormatter NUM_FMT = DateTimeFormat.forPattern("YYYYMMddHHmmss");




    /**
     * 同步支付宝账务
      * @param start 开始时间
     * @param end 截止时间
     * @param pageNo 第几页
     * @param pageSize 每页数据数
     * @return 同步的数据记录数
     */
   public Response<Integer> syncAlipayTrans(Date start, Date end, Integer pageNo, Integer pageSize){

       Response<Integer> result = new Response<Integer>();


       try {

           AlipayToken token = getAlipayToken();

           AlipayManager alipayManager = new AlipayManager();

           RedirectInfo info = alipayManager.syncThridTrans(start,end,pageNo,pageSize,token);
           if(!info.isSuccess()){
               throw new ServiceException(info.getError());
           }

           log.debug("query url: {}", info.getResult());
           String body = HttpRequest.get(info.getResult()).connectTimeout(10000).readTimeout(10000).body();
           log.debug("return body content: {}", body);


           AlipaySettlementResponse resp = PageQueryRequest.queryResultToObject(body);

           if(resp==null || resp.getResult()==null || resp.getResult().getPaging()==null || resp.getResult().getPaging().getAccountLogList()==null){
               result.setResult(0);
               return result;
           }
           for (AlipaySettlementDto alipaySettlementDto : resp.getResult().getPaging().getAccountLogList()) {

               AlipayTrans trans = BeanMapper.map(alipaySettlementDto, AlipayTrans.class);

               trans.setTradeAt(STD_FMT.parseDateTime(trans.getTransDate()).toDate());
               Response<Boolean> createRes = payWriteService.createAlipayTrans(trans);
               if(!createRes.isSuccess()){
                   log.error("create alipay trans: {} fail,error: {} ",trans, createRes.getError());
               }
           }

           result.setResult(resp.getResult().getPaging().getAccountLogList().size());

       }catch (ServiceException e){
           log.error("sync alipay trans fail start:{} end: {} ,error: {}",start,end,e.getMessage());
           result.setError(e.getMessage());

       }catch (Exception e){
           log.error("sync alipay trans fail start:{} end: {} ,cause: {}",start,end,Throwables.getStackTraceAsString(e));
           result.setError("sync.alipay.trans.fail");
       }

       return result;




   }



    private AlipayToken getAlipayToken(){

        if (!configCenter.get(AlipaySettings.ACCOUNT).isPresent()) {
            throw new ServiceException("config."+AlipaySettings.ACCOUNT+".not.exist");
        }
        if (!configCenter.get(AlipaySettings.GATEWAY).isPresent()) {
            throw new ServiceException("config."+AlipaySettings.GATEWAY+".not.exist");
        }
        if (!configCenter.get(AlipaySettings.KEY).isPresent()) {
            throw new ServiceException("config."+AlipaySettings.KEY+".not.exist");
        }
        if (!configCenter.get(AlipaySettings.PID).isPresent()) {
            throw new ServiceException("config."+AlipaySettings.PID+".not.exist");
        }
        if (!configCenter.get(AlipaySettings.WAP_GATEWAY).isPresent()) {
            throw new ServiceException("config."+AlipaySettings.WAP_GATEWAY+".not.exist");
        }


        AlipayToken token = new AlipayToken();
        token.setAccount(configCenter.get(AlipaySettings.ACCOUNT).get());
        token.setGateway(configCenter.get(AlipaySettings.GATEWAY).get());
        token.setKey(configCenter.get(AlipaySettings.KEY).get());
        token.setPid(configCenter.get(AlipaySettings.PID).get());
        token.setWapGateway(configCenter.get(AlipaySettings.WAP_GATEWAY).get());

        return token;

    }

    public Response<Integer> syncWechatpayAccountDetails(Date billDate, WxDownloadBillRequest.BillType billType) {

        Response<Integer> result = new Response<Integer>();

        try {

            WxToken token = getWxToken();

            WechatpayManager manager = new WechatpayManager();
            RedirectInfo info = manager.syncThridTrans(billDate, billType, token);

            if(!info.isSuccess()){
                throw new ServiceException(info.getError());
            }
            String body = HttpRequest.post(token.getDowloadBillUrl()).send(info.getResult()).trustAllCerts().trustAllHosts().body();
            log.debug("wx refund query body: {}", body);

            if(body.indexOf("xml") != -1){
                Map error = WxRequest.fromXML(body);
                result.setError(error.get("return_msg").toString());
                return result;
            }
            WxPayBillResponse response = WxDownloadBillRequest.parseBillContent(body, billType.getValue());

            for (WxPayBillDto payBillDto : response.getWxPayBillDtos()) {
                WechatpayTrans wechatpayTrans = BeanMapper.map(payBillDto, WechatpayTrans.class);
                wechatpayTrans.setTradeTime(STD_FMT.print(payBillDto.getTradeTime().getTime()));
                wechatpayTrans.setTradeAt(payBillDto.getTradeTime());
                if (payBillDto.getRefundApplyDate() != null) {
                    wechatpayTrans.setRefundApplyDate(STD_FMT.print(payBillDto.getRefundApplyDate().getTime()));
                }
                if (payBillDto.getRefundSuccessDate() != null) {
                    wechatpayTrans.setRefundSuccessDate(STD_FMT.print(payBillDto.getRefundSuccessDate().getTime()));
                }
                Response<Boolean> createRes = payWriteService.createWechatPayTrans(wechatpayTrans);
                if(!createRes.isSuccess()){
                    log.error(createRes.getError());
                }
            }

        } catch (Exception e) {
            log.error("sync wechat pay account detail fail bill date: {} billtype:{} cause:{}",billDate,billType.getValue(),Throwables.getStackTraceAsString(e));
            result.setError("sysnc.wechat.account.detail.fail");

        }


        return result;
    }


    private WxToken getWxToken(){


        if (!configCenter.get(WechatSettings.APP_ID).isPresent()) {
            throw new ServiceException("config."+WechatSettings.APP_ID+".not.exist");
        }
        if (!configCenter.get(WechatSettings.SECRET).isPresent()) {
            throw new ServiceException("config."+WechatSettings.SECRET+".not.exist");
        }
        if (!configCenter.get(WechatSettings.MCHID).isPresent()) {
            throw new ServiceException("config."+WechatSettings.MCHID+".not.exist");
        }
        if (!configCenter.get(WechatSettings.PATERNERKEY).isPresent()) {
            throw new ServiceException("config."+WechatSettings.PATERNERKEY+".not.exist");
        }

        if (!configCenter.get(WechatSettings.WECHAT_GATEWAY).isPresent()) {
            throw new ServiceException("config."+WechatSettings.WECHAT_GATEWAY+".not.exist");
        }

        if (!configCenter.get(WechatSettings.WECHAT_QUERY_REFUND_GATEWAY).isPresent()) {
            throw new ServiceException("config."+WechatSettings.WECHAT_QUERY_REFUND_GATEWAY+".not.exist");
        }

        if (!configCenter.get(WechatSettings.WECHAT_REFUND_GATEWAY).isPresent()) {
            throw new ServiceException("config."+WechatSettings.WECHAT_REFUND_GATEWAY+".not.exist");
        }

        if (!configCenter.get(WechatSettings.WECHAT_SETTLEMENT_GATEWAY).isPresent()) {
            throw new ServiceException("config."+WechatSettings.WECHAT_SETTLEMENT_GATEWAY+".not.exist");
        }

        WxToken token = new WxToken();
        token.setAppId(configCenter.get(WechatSettings.APP_ID).get());
        token.setSecret(configCenter.get(WechatSettings.SECRET).get());
        token.setMchId(configCenter.get(WechatSettings.MCHID).get());
        token.setPaternerkey(configCenter.get(WechatSettings.PATERNERKEY).get());
        token.setGateway(configCenter.get(WechatSettings.WECHAT_GATEWAY).get());
        token.setQueryRefundGateway(configCenter.get(WechatSettings.WECHAT_QUERY_REFUND_GATEWAY).get());
        token.setRefundGateway(configCenter.get(WechatSettings.WECHAT_REFUND_GATEWAY).get());
        token.setDowloadBillUrl(configCenter.get(WechatSettings.WECHAT_SETTLEMENT_GATEWAY).get());

        return token;

    }

    public KjtpayTrans initTrans(List<String> list){
        KjtpayTrans kjtpayTrans = new KjtpayTrans();
        kjtpayTrans.setAmount(list.get(6));
        kjtpayTrans.setInnerNo(list.get(2));
        kjtpayTrans.setOuterNo(list.get(0));
        kjtpayTrans.setOrigOuterNo(list.get(1));
        kjtpayTrans.setType(list.get(3));
        kjtpayTrans.setOrderAt(DateTime.parse(list.get(4), NUM_FMT).toDate());//list.get(4)
        kjtpayTrans.setPaidAt(DateTime.parse(list.get(5), NUM_FMT).toDate());//list.get(5)
        kjtpayTrans.setRate(calcRate(list.get(6), list.get(7)));   //目前费率是计算出来的
        kjtpayTrans.setRateFee(list.get(7));
        kjtpayTrans.setStatus(list.get(8));
        kjtpayTrans.setCreatedAt(new Date());
        kjtpayTrans.setUpdatedAt(new Date());

        return kjtpayTrans;
    }

    //手动计算费率
    private static String calcRate(String amount, String rateFee) {
        BigDecimal kjtAmount = new BigDecimal(amount);
        BigDecimal kjtRateFee = new BigDecimal(rateFee);

        BigDecimal result = kjtRateFee.divide(kjtAmount, 4, BigDecimal.ROUND_UP); //保留小数点后面4位，向上取整！
        return result.toString();
    }
}
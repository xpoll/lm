package io.terminus.galaxy.schedule.service;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.BeanMapper;
import io.terminus.lib.pay.channel.alipay.config.AlipaySettings;
import io.terminus.lib.pay.channel.alipay.dto.AlipaySettlementResponse;
import io.terminus.lib.pay.channel.alipay.dto.RedirectInfo;
import io.terminus.lib.pay.channel.alipay.dto.settlement.AlipaySettlementDto;
import io.terminus.lib.pay.channel.alipay.manager.AlipayManager;
import io.terminus.lib.pay.channel.alipay.request.AlipayToken;
import io.terminus.lib.pay.channel.alipay.request.PageQueryRequest;
import io.terminus.lib.pay.channel.kjtpay.config.KjtpaySettings;
import io.terminus.lib.pay.channel.kjtpay.manager.KjtpayManager;
import io.terminus.lib.pay.channel.kjtpay.request.KjtPageQueryRequest;
import io.terminus.lib.pay.channel.kjtpay.request.KjtToken;
import io.terminus.lib.pay.channel.unionpay.config.UnionpaySettings;
import io.terminus.lib.pay.channel.unionpay.manager.UnionpayManager;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import io.terminus.lib.pay.channel.unionpay.sdk.SDKConfig;
import io.terminus.lib.pay.channel.wechatpay.config.WechatSettings;
import io.terminus.lib.pay.channel.wechatpay.dto.WxPayBillDto;
import io.terminus.lib.pay.channel.wechatpay.dto.WxPayBillResponse;
import io.terminus.lib.pay.channel.wechatpay.manager.WechatpayManager;
import io.terminus.lib.pay.channel.wechatpay.request.WxDownloadBillRequest;
import io.terminus.lib.pay.channel.wechatpay.request.WxRequest;
import io.terminus.lib.pay.channel.wechatpay.request.WxToken;
import io.terminus.parana.config.ConfigCenter;
import io.terminus.parana.pay.model.AlipayTrans;
import io.terminus.parana.pay.model.KjtpayTrans;
import io.terminus.parana.pay.model.UnionPayTrans;
import io.terminus.parana.pay.model.WechatpayTrans;
import io.terminus.parana.pay.service.PayWriteService;
import lombok.extern.slf4j.Slf4j;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;

import static com.google.common.base.Preconditions.checkState;
import static io.terminus.common.utils.Arguments.isNullOrEmpty;

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



    public Response<Integer> syncUnionPayTrans(Date billDate) {
        Response<Integer> result = new Response<Integer>();

        try {
            UnionToken unionToken = getUnionToken();

            UnionpayManager unionpayManager = new UnionpayManager();
            Response<List<Map<Integer,String>>> response = unionpayManager.sysncThirdTrans(billDate, unionToken);
            if(!response.isSuccess()){
                throw new ServiceException(response.getError());
            }

            result.setResult(createUnionPayTrans(response.getResult(),unionpayManager));

        }catch (Exception e){
            result.setError("sync.union.pay.fail");
            log.error("sysc union pay trans fail:{}",Throwables.getCausalChain(e));
        }

        return result;
    }


    private UnionToken getUnionToken(){

        initUnionPayConfig();

        if (!configCenter.get(UnionpaySettings.MERId).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.MERId+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.FRONT_TRANS_URL).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.FRONT_TRANS_URL+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.BACK_TRANS_URL).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.BACK_TRANS_URL+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.FILE_TRANS_URL).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.FILE_TRANS_URL+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.NOTIFY).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.NOTIFY+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.RETURN_URL).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.RETURN_URL+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.REFUND_NOTIFY).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.REFUND_NOTIFY+".not.exist");
        }

        UnionToken token = new UnionToken();
        token.setMerId(configCenter.get(UnionpaySettings.MERId).get());
        token.setFrontTransUrl(configCenter.get(UnionpaySettings.FRONT_TRANS_URL).get());
        token.setBackTransUrl(configCenter.get(UnionpaySettings.BACK_TRANS_URL).get());
        token.setFileTransUrl(configCenter.get(UnionpaySettings.FILE_TRANS_URL).get());

        return token;

    }


    public void initUnionPayConfig(){

        SDKConfig config = SDKConfig.getConfig();
        if(!Strings.isNullOrEmpty(config.getSignCertPath())){
            return;
        }

        if (!configCenter.get(UnionpaySettings.PFX_PATH).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.PFX_PATH+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.SIGN_CERT_PASSWORD).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.SIGN_CERT_PASSWORD+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.SIGN_CERT_TYPE).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.SIGN_CERT_TYPE+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.CER_PATH).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.CER_PATH+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.SINGLE_Mode).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.SINGLE_Mode+".not.exist");
        }
        if (!configCenter.get(UnionpaySettings.VALIDATE_CERT_DIR).isPresent()) {
            throw new ServiceException("config."+UnionpaySettings.VALIDATE_CERT_DIR+".not.exist");
        }
        config.setSignCertPath(configCenter.get(UnionpaySettings.PFX_PATH).get());
        config.setSignCertPwd(configCenter.get(UnionpaySettings.SIGN_CERT_PASSWORD).get());
        config.setSignCertType(configCenter.get(UnionpaySettings.SIGN_CERT_TYPE).get());
        config.setValidateCertDir(configCenter.get(UnionpaySettings.VALIDATE_CERT_DIR).get());
        config.setEncryptCertPath(configCenter.get(UnionpaySettings.CER_PATH).get());
        config.setSingleMode(configCenter.get(UnionpaySettings.SINGLE_Mode).get());
        config.setTransFilePatch(configCenter.get(UnionpaySettings.FILE_TRANS_URL).get());

    }

    private Integer createUnionPayTrans(List<Map<Integer,String>> maps,UnionpayManager unionpayManager){

        if(isNullOrEmpty(maps)){
            return 0;
        }

        for (Map<Integer,String> map : maps){
            UnionPayTrans unionPayTrans = new UnionPayTrans();
            unionPayTrans.setTransactionCode(map.get(0));
            unionPayTrans.setAcqInsCode(map.get(1));
            unionPayTrans.setSendCode(map.get(2));
            unionPayTrans.setTraceNo(map.get(3));
            String year = DFT.print(new DateTime());
            unionPayTrans.setTxnTime(DateTime.parse(year + map.get(4), DFT2).toDate());
            unionPayTrans.setPayCardNo(map.get(5));
            unionPayTrans.setTxnAmt(unionpayManager.getFee(map.get(6)));
            unionPayTrans.setMerCatCode(map.get(7));
            unionPayTrans.setTermType(map.get(8));
            unionPayTrans.setQueryId(map.get(9));
            unionPayTrans.setType(map.get(10));
            unionPayTrans.setOrderId(map.get(11));
            unionPayTrans.setPayCardType(map.get(12));
            unionPayTrans.setOriginalTraceNo(map.get(13));
            unionPayTrans.setOriginalTime(map.get(14));
            unionPayTrans.setThirdPartyFee(unionpayManager.getFee(map.get(15)));
            unionPayTrans.setSettleAmount(map.get(16));
            unionPayTrans.setPayType(map.get(17));
            unionPayTrans.setCompanyCode(map.get(18));
            unionPayTrans.setTxnType(map.get(19));
            unionPayTrans.setTxnSubType(map.get(20));
            unionPayTrans.setBizType(map.get(21));
            unionPayTrans.setAccType(map.get(22));
            unionPayTrans.setBillType(map.get(23));
            unionPayTrans.setBillNo(map.get(24));
            unionPayTrans.setInteractMode(map.get(25));
            unionPayTrans.setOrigQryId(map.get(26));
            unionPayTrans.setMerId(map.get(27));
            unionPayTrans.setDivideType(map.get(28));
            unionPayTrans.setSubMerId(map.get(29));
            unionPayTrans.setSubMerAbbr(map.get(30));
            unionPayTrans.setDivideAmount(map.get(31));
            unionPayTrans.setClearing(map.get(32));
            unionPayTrans.setTermId(map.get(33));
            unionPayTrans.setMerReserved(map.get(34));
            unionPayTrans.setDiscount(map.get(35));
            unionPayTrans.setInvoice(map.get(36));
            unionPayTrans.setAdditionThirdPartyFee(map.get(37));
            unionPayTrans.setStage(map.get(38));
            unionPayTrans.setTransactionMedia(map.get(39));
            unionPayTrans.setOriginalOrderId(map.get(40));
            Response<Boolean> response = payWriteService.createUnionpayTrans(unionPayTrans);
            if(!response.isSuccess()){
                log.error(response.getError());
            }
        }

        return maps.size();
    }




    public Response<Boolean> syncKjtpayTrans(Date date, Integer pageNo) {

        Response<Boolean> result = new Response<Boolean>();

        try {
            KjtToken kjtToken = getKjtToken();
            String body = KjtPageQueryRequest.build(kjtToken).
                    settleDate(date).
                    pageNo(pageNo.toString()).query();

            if(Strings.isNullOrEmpty(body)){
                result.setError("");
                return result;

            }
            //解析字符串
            Splitter.MapSplitter splitter = Splitter.on("&").withKeyValueSeparator("=");
            Map<String, String> map = splitter.split(body);
            checkState("T".equals(map.get("is_success")), "query.kjtpay.trans.fail");
            if(map.containsKey("settled_data")){
                List<String> lists = Splitter.on("$").splitToList(map.get("settled_data"));
                log.debug("kjtpay trans sync date:{}, info:{}", date, lists);
                for(String st : lists){
                    List<String> items = Splitter.on("~").splitToList(st);
                    KjtpayTrans kjtpayTrans = initTrans(items);


                    Response<Boolean> response = payWriteService.createKjtpayTrans(kjtpayTrans);
                    if(!response.isSuccess()){
                        log.error(response.getError());
                    }
                }
            }
            result.setResult("true".equals(map.get("has_next_page")) ? Boolean.TRUE : Boolean.FALSE);

        } catch (Exception e) {
            log.error("Sync kjtpay trans failed", e);
        }
        return result;
    }


    private KjtToken getKjtToken(){


        if (!configCenter.get(KjtpaySettings.PID).isPresent()) {
            throw new ServiceException("config."+KjtpaySettings.PID+".not.exist");
        }
        if (!configCenter.get(KjtpaySettings.PFX_PATH).isPresent()) {
            throw new ServiceException("config."+KjtpaySettings.PFX_PATH+".not.exist");
        }
        if (!configCenter.get(KjtpaySettings.PASSWORD).isPresent()) {
            throw new ServiceException("config."+KjtpaySettings.PASSWORD+".not.exist");
        }
        if (!configCenter.get(KjtpaySettings.ACCOUNT).isPresent()) {
            throw new ServiceException("config."+KjtpaySettings.ACCOUNT+".not.exist");
        }
        if (!configCenter.get(KjtpaySettings.CER_PATH).isPresent()) {
            throw new ServiceException("config."+KjtpaySettings.CER_PATH+".not.exist");
        }
        if (!configCenter.get(KjtpaySettings.GATEWAY).isPresent()) {
            throw new ServiceException("config."+KjtpaySettings.GATEWAY+".not.exist");
        }

        KjtToken token = new KjtToken();
        token.setPid(configCenter.get(KjtpaySettings.PID).get());
        token.setPfxPath(configCenter.get(KjtpaySettings.PFX_PATH).get());
        token.setKeyPassword(configCenter.get(KjtpaySettings.PASSWORD).get());
        token.setAccount(configCenter.get(KjtpaySettings.ACCOUNT).get());
        token.setCerPath(configCenter.get(KjtpaySettings.CER_PATH).get());
        token.setGateway(configCenter.get(KjtpaySettings.GATEWAY).get());

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
package io.terminus.galaxy.open.rest.user;

import com.github.cage.Cage;
import com.github.cage.token.RandomTokenGenerator;
import com.google.common.base.Charsets;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.hash.Hashing;
import io.terminus.boot.session.properties.SessionProperties;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.common.enums.UserRole;
import io.terminus.galaxy.common.enums.UserStatus;
import io.terminus.galaxy.common.enums.UserType;
import io.terminus.galaxy.open.common.CaptchaGenerator;
import io.terminus.galaxy.open.common.Sessions;
import io.terminus.galaxy.web.core.component.MobilePattern;
import io.terminus.galaxy.web.core.util.TextValidator;
import io.terminus.lib.sms.SmsException;
import io.terminus.pampas.common.UserUtil;
import io.terminus.pampas.openplatform.annotations.OpenBean;
import io.terminus.pampas.openplatform.annotations.OpenMethod;
import io.terminus.pampas.openplatform.exceptions.OPClientException;
import io.terminus.parana.user.model.LoginType;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.model.UserDevice;
import io.terminus.parana.user.service.DeviceWriteService;
import io.terminus.parana.user.service.UserReadService;
import io.terminus.parana.user.service.UserWriteService;
import io.terminus.parana.web.msg.MsgWebService;
import io.terminus.session.AFSessionManager;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.Base64Utils;

import java.io.Serializable;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkState;
import static com.google.common.base.Strings.isNullOrEmpty;
import static io.terminus.common.utils.Arguments.equalWith;
import static io.terminus.common.utils.Arguments.isEmpty;
import static io.terminus.common.utils.Arguments.isNull;
import static io.terminus.common.utils.Arguments.notEmpty;

/**
 * Mail: xiao@terminus.io <br>
 * Date: 2016-03-15 4:06 PM  <br>
 * Author: xiao
 */
@Slf4j
@OpenBean
@SuppressWarnings("all")
public class OPUsers {
    public static final Character SEP='Z';
    private final String hostIpMd5;

    @Autowired
    private CaptchaGenerator captchaGenerator;
    private final Cage cage = new Cage();
    @Autowired
    private AFSessionManager sessionManager;
    @Autowired
    private SessionProperties sessionProperties;
    @Autowired
    private UserReadService<User> userReadService;
    @Autowired
    private UserWriteService<User> userWriteService;
    @Autowired
    private DeviceWriteService deviceWriteService;
    @Autowired
    private MobilePattern mobilePattern;
    @Autowired
    private MsgWebService smsWebService;

    public OPUsers() {
        String hostIp;
        try {
            hostIp = InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException e) {
            hostIp = UUID.randomUUID().toString();
        }
        hostIpMd5 = Hashing.md5().hashString(hostIp, Charsets.UTF_8).toString().substring(0,8);
    }

    @OpenMethod(key = "get.session.id", paramNames = {"key"})
    public Map<String, String> getSessionId(String key) {
        if (isEmpty(key)) {
            throw new OPClientException("key.miss");
        }
        return ImmutableMap.of("sessionId", generateId(key));
    }

    private String generateId(String key) {
        StringBuilder builder = new StringBuilder(30);
        String clientKey =  Hashing.md5().hashString(key, Charsets.UTF_8).toString().substring(0, 8);
        builder.append(clientKey).append(SEP).append(hostIpMd5)
                .append(SEP).append(Long.toHexString(System.currentTimeMillis()))
                .append(SEP).append(UUID.randomUUID().toString().substring(0,4));
        return builder.toString();
    }

    @OpenMethod(key="server.time", paramNames = {})
    public Map<String, String> serverTime() {
        return ImmutableMap.of("time", DateTime.now().toString(DateTimeFormat.forPattern("yyyyMMddHHmmss")));
    }

    @OpenMethod(key="user.login", paramNames = {"name", "password", "type", "code", "sid"})
    public Token login(String name, String password, String type, String code, String sessionId) {
        if (isEmpty(name)) {
            throw new OPClientException("user.name.miss");
        }
        if (isEmpty(password)) {
            throw new OPClientException("user.password.miss");
        }
        if (isEmpty(sessionId)) {
            throw new OPClientException("session.id.miss");
        }

        // 当用户次数超过指定次数之后,需要校验code
        if (arriveErrorLimit(sessionId)) {
            if (isEmpty(code)) {
                throw new OPClientException("user.code.miss");
            }
        }
        // 判断CODE是否匹配
        Map<String, Object> snapshot = sessionManager.findSessionById(Sessions.CODE_PREFIX, sessionId);
        if (code != null && snapshot.get("code") != null && !snapshot.get("code").equals(code)) {
            throw new OPClientException("user.code.mismatch");
        }

        // 用户名密码登录
        User user = doLogin(name, password, type, sessionId);
        // 登录成功记录 session
        sessionManager.save(Sessions.TOKEN_PREFIX, sessionId, ImmutableMap.of(Sessions.USER_ID, (Object) user.getId()), Sessions.LONG_INACTIVE_INTERVAL);
        // 清除 limit & code
        sessionManager.deletePhysically(Sessions.LIMIT_PREFIX, sessionId);
        sessionManager.deletePhysically(Sessions.CODE_PREFIX, sessionId);

        // 返回登录的凭证
        Token token = new Token();
        token.setName(name);
        token.setDomain(sessionProperties.getCookieDomain());
        token.setExpiredAt(DateTime.now().plusSeconds(Sessions.LONG_INACTIVE_INTERVAL)
                .toString(DateTimeFormat.forPattern("yyyyMMddHHmmss")));
        token.setSessionId(sessionId);
        token.setCookieName(sessionProperties.getCookieName());
        return token;
    }

    private User doLogin(String name, String password, String type, String sessionId) {
        LoginType loginType = isNull(type) ? LoginType.NAME : LoginType.from(Integer.parseInt(type));
        Response<User> result = userReadService.login(name, password, loginType);
        if (!result.isSuccess()) {
            plusErrorCount(sessionId);
            refreshCaptcher(sessionId);
            // 登录失败, 记录登录失败次数
            throw new OPClientException(result.getError());
        }
        return result.getResult();
    }

    private void plusErrorCount(String sessionId) {
        Map<String, Object> snapshot = sessionManager.findSessionById(Sessions.LIMIT_PREFIX, sessionId);
        boolean arriveLimit = false;

        if (snapshot.size() == 0) {
            snapshot = Maps.newHashMap();
            snapshot.put("count", 1);
        } else {
            Integer count = (Integer) snapshot.get("count");
            count = count + 1;
            snapshot.put("count", count);
            if (count > 3) {
                arriveLimit = true;
            }
        }
        sessionManager.save(Sessions.LIMIT_PREFIX, sessionId, snapshot, Sessions.MIDDLE_INACTIVE_INTERVAL);
    }

    private boolean arriveErrorLimit(String sessionId) {
        Map<String, Object> snapshot = sessionManager.findSessionById(Sessions.LIMIT_PREFIX, sessionId);
        boolean arriveLimit = false;

        if (snapshot.size() >= 0 && snapshot.get("count") != null) {
            Integer count = (Integer) snapshot.get("count");
            if (count > 2) {
                arriveLimit = true;
            }
        }
        return arriveLimit;
    }

    @OpenMethod(key="get.user.captcher", paramNames = {"sid"})
    public Map<String, String> captcher(String sessionId) {
        if (isEmpty(sessionId)) {
            throw new OPClientException("session.id.miss");
        }
        String code = refreshCaptcher(sessionId);
        byte[] data = captchaGenerator.captcha(code);
        return ImmutableMap.of("captcher", Base64Utils.encodeToString(data));
    }

    private String refreshCaptcher(String sessionId) {
        // 将图片验证码存入session
        RandomTokenGenerator tokenGenerator = new RandomTokenGenerator(null, 4, 2);
        String code = tokenGenerator.next();
        Map<String, Object> snapshot = Maps.newHashMap();
        snapshot.put("code", code);
        sessionManager.save(Sessions.CODE_PREFIX, sessionId, snapshot, Sessions.SHORT_INACTIVE_INTERVAL);
        return code;
    }

    @OpenMethod(key="user.device.bind", paramNames = {"deviceToken"})
    public void bindDevice(String deviceToken) {
        if (isEmpty(deviceToken)) {
            throw new OPClientException("device.token.miss");
        }
        UserDevice userDevice = new UserDevice();
        userDevice.setUserId(UserUtil.getUserId());
        userDevice.setUserName(UserUtil.getCurrentUser().getName());
        userDevice.setDeviceToken(deviceToken);
        userDevice.setDeviceType("app");
        Response<Long> res = deviceWriteService.create(userDevice);
        if (!res.isSuccess()) {
            throw new OPClientException(res.getError());
        }
    }

    @OpenMethod(key="user.logout", paramNames = {"sid"})
    public void logout(String sessionId) {
        sessionManager.deletePhysically(Sessions.TOKEN_PREFIX, sessionId);

        // unbind user device
        Long uid = UserUtil.getUserId();
        Response<Integer> res = deviceWriteService.deleteByUserIdAndDeviceType(uid, "app");
        if (!res.isSuccess()) {
            throw new OPClientException("user.device.unbind.fail");
        }
    }


    @OpenMethod(key="user.sign", paramNames = {"sid", "username", "email", "mobile", "code", "password"})
    public User userSign(String sid, String username, String email, String mobile, String code, String password) {
        if (isNullOrEmpty(password)) {
            throw new OPClientException("user.register.password.empty");
        }
        if (!password.matches("[\\s\\S]{6,16}")) {
            throw new OPClientException("user.password.6to16");
        }
        User user = null;
        if (!isNullOrEmpty(mobile)) {
            if (isNullOrEmpty(code)) {
                throw new OPClientException("user.register.mobile.code.empty");
            }
            validateSmsCode(code, mobile, sid);
            user = registerByMobile(mobile, password, username);
        } else if (!isNullOrEmpty(email)) {
            if(!TextValidator.EMAIL.boolCheck(email)){
                throw new OPClientException("user.register.email.invalid");
            }
            user = registerByEmail(email, password, username);
        } else if (!isNullOrEmpty(username)) {
            throw new OPClientException("user.register.username.no.supported");
        } else {
            throw new OPClientException("user.register.no.name");
        }
        return user;
    }

    @OpenMethod(key="user.sign.sms.send", paramNames = {"sid", "mobile"})
    public void sendSignSms(String sid, String mobile) {
        if (!mobilePattern.getPattern().matcher(mobile).matches()) {
            throw new OPClientException("mobile.format.error");
        }
        Map<String, Object> snapshot = sessionManager.findSessionById(Sessions.CODE_PREFIX, sid);

        // session verify, value = code@time@mobile
        String activateCode = (String) snapshot.get("sms");

        Response<Boolean> result = null;
        if (!Strings.isNullOrEmpty(activateCode)) { //判断是否需要重新发送激活码
            List<String> parts = AT_SPLITTER.splitToList(activateCode);
            long sendTime = Long.parseLong(parts.get(1));
            if (System.currentTimeMillis() - sendTime < TimeUnit.MINUTES.toMillis(1)) { //
                log.error("could not send sms, sms only can be sent once in one minute");
                throw new OPClientException("1分钟内只能获取一次验证码");
            }
            String code = String.valueOf((int)((Math.random()*9+1)*100000));
            snapshot.put("code", code + "@" + System.currentTimeMillis()+"@"+mobile);
            // 发送验证码
            result = doSendSms(code, mobile);
        } else { //新发送激活码
            String code = String.valueOf((int) ((Math.random() * 9 + 1) * 100000));
            snapshot.put("code", code + "@" + System.currentTimeMillis()+"@"+mobile);
            // 发送验证码
            result = doSendSms(code, mobile);
        }
        if(!result.isSuccess()) {
            log.warn("send sms single fail, cause:{}", result.getError());
            throw new OPClientException(result.getError());
        }
        sessionManager.save(Sessions.CODE_PREFIX, sid, snapshot, Sessions.LONG_INACTIVE_INTERVAL);
    }

    /**
     * 发送短信验证码
     *
     * @param code   验证码
     * @param mobile 手机号
     * @return 发送结果
     */
    private Response<Boolean> doSendSms(String code, String mobile){
        Response<Boolean> r = new Response<Boolean>();
        try {
            Map<String, Serializable> context=new HashMap<>();
            context.put("code",code);
            String result=smsWebService.send(mobile, "user.register.code", context, null);
            log.info("send sms result : {}", result);
            r.setResult(Boolean.TRUE);
            return r;
        }catch (SmsException e) {
            log.info("send sms failed, error : {} ", e.getMessage());
            throw new JsonResponseException(500, "sms.send.fail");
        }catch (Exception e) {
            log.error("send sms failed , error : {}", e.getMessage());
            throw new JsonResponseException(500, "sms.send.fail");
        }
    }

    private final Splitter AT_SPLITTER = Splitter.on('@').trimResults();

    /**
     * 校验手机验证码
     *
     * @param code    输入的验证码
     * @param request 请求
     */
    private void validateSmsCode(String code, String mobile, String sessionId) {
        Map<String, Object> snapshot = sessionManager.findSessionById(Sessions.CODE_PREFIX, sessionId);

        // session verify, value = code@time@mobile
        String codeInSession = (String) snapshot.get("sms");
        checkArgument(notEmpty(codeInSession), "sms.token.error");
        String expectedCode = AT_SPLITTER.splitToList(codeInSession).get(0);
        checkArgument(equalWith(code, expectedCode), "sms.token.error");
        String expectedMobile = AT_SPLITTER.splitToList(codeInSession).get(2);
        checkArgument(equalWith(mobile, expectedMobile), "sms.token.error");

        // 如果验证成功则删除之前的code
        sessionManager.deletePhysically(Sessions.CODE_PREFIX, sessionId);
    }

    /**
     * 手机注册
     *
     * @param mobile 手机号
     * @param password 密码
     * @param userName 用户名
     * @return 注册成功之后的用户
     */
    private User registerByMobile(String mobile, String password, String userName) {
        Response<User> result = userReadService.findBy(mobile, LoginType.MOBILE);
        // 检测手机号是否已存在
        if(result.isSuccess() && result.getResult() != null){
            throw new JsonResponseException("user.register.mobile.has.been.used");
        }
        // 设置用户信息
        User user = new User();
        user.setMobile(mobile);
        user.setPassword(password);
        user.setName(userName);

        // 用户状态 0: 未激活, 1: 正常, -1: 锁定, -2: 冻结, -3: 删除
        user.setStatus(UserStatus.NORMAL.value());
        // 用户类型 1: 超级管理员, 2: 普通用户, 3: 后台运营, 4: 站点拥有者
        user.setType(UserType.NORMAL.value());
        // 注册用户默认成为个人买家
        user.setRoles(Lists.newArrayList(UserRole.BUYER.name()));

        Response<Long> resp = userWriteService.create(user);
        checkState(resp.isSuccess(), resp.getError());
        user.setId(resp.getResult());
        return user;
    }

    /**
     * 邮箱注册
     *
     * @param email    邮箱
     * @param password 密码
     * @param userName 用户名
     * @return 注册成功之后用户
     */
    public User registerByEmail(String email, String password, String userName){
        Response<User> result = userReadService.findBy(email, LoginType.EMAIL);
        // 检测邮箱是否已存在
        if(result.isSuccess() && result.getResult() != null){
            throw new JsonResponseException("user.register.email.has.been.used");
        }
        // 设置用户信息
        User user = new User();
        user.setEmail(email);
        user.setPassword(password);
        user.setName(userName);

        // 邮箱注册需要激活之后才能用
        // 用户状态 0: 未激活, 1: 正常, -1: 锁定, -2: 冻结, -3: 删除
        user.setStatus(UserStatus.NOT_ACTIVATE.value());
        // 用户类型 1: 超级管理员, 2: 普通用户, 3: 后台运营, 4: 站点拥有者
        user.setType(UserType.NORMAL.value());
        // 注册用户默认成为个人买家
        user.setRoles(Lists.newArrayList(UserRole.BUYER.name()));

        Response<Long> resp = userWriteService.create(user);
        checkState(resp.isSuccess(), resp.getError());
        user.setId(resp.getResult());
        return user;
    }

    @Data
    class Token implements Serializable {
        String name;
        String expiredAt;
        String sessionId;
        String cookieName;
        String domain;
    }
}

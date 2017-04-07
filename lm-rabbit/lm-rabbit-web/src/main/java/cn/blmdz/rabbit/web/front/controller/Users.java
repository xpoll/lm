/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.web.front.controller;

import static cn.blmdz.home.common.util.Arguments.equalWith;
import static cn.blmdz.home.common.util.Arguments.isNull;
import static cn.blmdz.home.common.util.Arguments.notEmpty;
import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkState;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.CharMatcher;
import com.google.common.base.Objects;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.eventbus.EventBus;

import cn.blmdz.aide.sms.SmsException;
import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.home.session.util.WebUtil;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.rabbit.common.enums.UserRole;
import cn.blmdz.rabbit.common.enums.UserStatus;
import cn.blmdz.rabbit.common.enums.UserType;
import cn.blmdz.rabbit.user.util.GalaxyUserMaker;
import cn.blmdz.rabbit.web.core.component.CaptchaGenerator;
import cn.blmdz.rabbit.web.core.component.MobilePattern;
import cn.blmdz.rabbit.web.core.util.TextValidator;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.common.utils.EncryptUtil;
import cn.blmdz.wolf.common.utils.RespHelper;
import cn.blmdz.wolf.common.utils.Strs;
import cn.blmdz.wolf.user.model.LoginType;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;
import cn.blmdz.wolf.user.service.UserWriteService;
import cn.blmdz.wolf.web.core.Constants;
import cn.blmdz.wolf.web.core.events.user.LoginEvent;
import cn.blmdz.wolf.web.core.events.user.LogoutEvent;
import cn.blmdz.wolf.web.core.events.user.RegisterEvent;
import cn.blmdz.wolf.web.msg.MsgWebService;
import lombok.extern.slf4j.Slf4j;

/**
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-01-30
 */
@Slf4j
@RestController
@RequestMapping("/api/user")
public class Users {

    // TODO 邮箱注册

    private final UserWriteService<User> userWriteService;

    private final UserReadService<User> userReadService;

    private final EventBus eventBus;

    private final CaptchaGenerator captchaGenerator;

    private final MobilePattern mobilePattern;

    private final MsgWebService smsWebService;

    private final Splitter AT_SPLITTER = Splitter.on('@').trimResults();

    /**
     * 该值用于标识当前的用户ID(不是用于登录)
     */
    private static final String UN_ACTIVE_USER_ID = "uaid";

    @Autowired
    public Users(UserWriteService<User> userWriteService,
                 UserReadService<User> userReadService,
                 EventBus eventBus,
                 CaptchaGenerator captchaGenerator,
                 MobilePattern mobilePattern,
                 @Qualifier("smsWebService") MsgWebService smsWebService) {
        this.userWriteService = userWriteService;
        this.userReadService = userReadService;
        this.eventBus = eventBus;
        this.captchaGenerator = captchaGenerator;
        this.mobilePattern = mobilePattern;
        this.smsWebService =smsWebService;
    }

    @RequestMapping("")
    public BaseUser getLoginUser() {
        return UserUtil.getCurrentUser();
    }

    /**
     * 用户注册
     *
     * @param password   密码
     * @param userName   用户名
     * @param email      邮箱
     * @param mobile     手机号
     * @param code       手机验证码
     * @param request    请求
     * @param response   响应
     * @return 注册成功之后的用户ID
     */
    @RequestMapping(value = "/register", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    public Long register(@RequestParam("password") String password,
                                   @RequestParam(value = "userName", required = false) String userName,
                                   @RequestParam(value = "email", required = false) String email,
                                   @RequestParam(value = "mobile", required = false) String mobile,
                                   @RequestParam(value = "code", required = false) String code,
                                   HttpServletRequest request,
                                   HttpServletResponse response){
        try {
            // 手机邮箱不可同时为空
            checkArgument(notEmpty(mobile) || notEmpty(email), "user.mobile.email.both.empty");
            // 如果是手机注册, 验证码不能为空
            if (notEmpty(mobile)) {
                checkArgument(notEmpty(code), "user.register.mobile.code.empty");
            }
            if (password.matches("[\\s\\S]{6,16}")) {
                Set<String> defaultParams = Sets.newHashSet("password", "userName", "email", "mobile");

                Map<String, Serializable> context = new HashMap<>();
                for (Map.Entry<String, String[]> paramEntry : request.getParameterMap().entrySet()) {
                    String key = paramEntry.getKey();
                    if (Strings.isNullOrEmpty(key) || defaultParams.contains(key)) {
                        continue;
                    }
                    if (paramEntry.getValue() != null && paramEntry.getValue().length > 0) {
                        // 只保留一个参数, 多传无益
                        context.put(key, paramEntry.getValue()[0]);
                    }
                }

                User user = null;

                if (notEmpty(email)) {
                    if(!TextValidator.EMAIL.boolCheck(email)){
                        throw new JsonResponseException("user.register.email.invalid");
                    }
                    user = registerByEmail(email, password, userName);
                    setUserIdInCookie(request, response, user.getId());
                } else {
                    // 校验手机验证码
                    validateSmsCode(code, mobile, request);
                    user = registerByMobile(mobile, password, userName);
                    // session and event
                    request.getSession().setAttribute(Constants.SESSION_USER_ID, user.getId());
                    eventBus.post(new RegisterEvent(null, null, GalaxyUserMaker.from(user)));
                }
                return user.getId();
            }else {
                throw new JsonResponseException(500,"user.password.6to16") ;
            }
        }catch (IllegalArgumentException e) {
            log.warn("failed to sign up userName={}, email={}, mobile={}, error:{}",
                    userName, email, mobile, e.getMessage());
            throw new JsonResponseException(500, e.getMessage());
        }catch (JsonResponseException e){
            log.warn("failed to sign up userName={}, email={}, mobile={}, error:{}",
                    userName, email, mobile, e.getMessage());
            throw new JsonResponseException(500, e.getMessage());
        }catch (Exception e) {
            log.error("failed to sign up userName={}, email={}, mobile={}, error:{}",
                    userName, email, mobile, Throwables.getStackTraceAsString(e));
            throw new JsonResponseException(500, "user.register.fail");
        }
    }

    @RequestMapping(value = "/register/mobile/validate")
    public Boolean registerValidate(@RequestParam String mobile){
        if (mobilePattern.getPattern().matcher(mobile).matches()) {
            User user = RespHelper.or500(userReadService.findBy(mobile, LoginType.MOBILE));
            if(Arguments.isNull(user)){
                return true;
            }
            return false;
        }
        return false;
    }



    /**
     * 登录
     */
    @RequestMapping(value = "/login", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Map<String, Object> login(@RequestParam("loginBy") String loginBy, @RequestParam("password") String password,
                                     @RequestParam(value = "target", required = false) String target,
                                     @RequestParam(value = "type", required = false) Integer type,
                                     HttpServletRequest request, HttpServletResponse response) {
        loginBy = loginBy.toLowerCase();
        LoginType loginType = isNull(type) ? LoginType.EMAIL : LoginType.from(type);
        Map<String, Object> map = new HashMap<>();

        Response<User> result = userReadService.login(loginBy, password, loginType);

        if (!result.isSuccess()) {
            log.warn("failed to login with(loginBy={}), error: {}", loginBy, result.getError());
            throw new JsonResponseException(500, result.getError());
        }

        User user = result.getResult();
        //判断当前用户是否激活
        if (Objects.equal(user.getStatus(), UserStatus.NOT_ACTIVATE.value())) {
            log.warn("user({}) isn't active", user);
        }
        request.getSession().setAttribute(Constants.SESSION_USER_ID, user.getId());

        LoginEvent loginEvent = new LoginEvent(request, response, GalaxyUserMaker.from(user));
        eventBus.post(loginEvent);
        target = !StringUtils.hasText(target)?"/":target;
        map.put("redirect",target);
        return map;
    }

    @RequestMapping(value = "/logout", method = RequestMethod.GET)
    public String logout(HttpServletRequest request, HttpServletResponse response) {
        try {
            HttpSession session = request.getSession(false);
            if (session != null) {
                session.invalidate();
            }
            ParanaUser loginUser = UserUtil.getCurrentUser();
            if (loginUser != null) {
                //delete login token cookie
                LogoutEvent logoutEvent = new LogoutEvent(request, response, loginUser);
                eventBus.post(logoutEvent);
            }
            return "/";
        } catch (Exception e) {
            log.error("failed to logout user,cause:", e);
            throw new JsonResponseException(500, "user.logout.fail");
        }
    }

    /**
     * 验证用户信息是否重复
     *
     * @param type      验证字段，有name，email，mobile
     * @param loginBy   输入内容
     * @param operation 1为创建时验证，2为修改时验证
     * @return 是否已存在
     */
    @RequestMapping(value = "/verify", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Boolean verifyBy(@RequestParam("type") Integer type,
                            @RequestParam("loginBy") String loginBy,
                            @RequestParam(value = "operation", defaultValue = "1") Integer operation) {
        LoginType loginType = LoginType.from(type);
        Long userId = UserUtil.getUserId();
        if (loginType == null) {
            throw new JsonResponseException("unknown login type:" + type);
        }
        if (!Objects.equal(operation, 1) && !Objects.equal(operation, 2)) {
            throw new JsonResponseException("unknown operation");
        }
        Response<User> result = userReadService.findBy(loginBy, loginType);
        if (Objects.equal(operation, 1)) {
            if (result.isSuccess()) {
                log.warn("user info {} already exists", loginBy);
                return false;
            }
        } else {
            if (result.isSuccess() && !Objects.equal(result.getResult().getId(), userId)) {
                log.warn("user info {} already exists", loginBy);
                return false;
            }
        }
        return true;
    }

    /**
     * 生成图片验证码
     *
     * @param request 请求
     * @return 图片验证码
     */
    @RequestMapping(value = "/imgVerify", method = RequestMethod.GET, produces = MediaType.IMAGE_JPEG_VALUE)
    @ResponseBody
    public ResponseEntity<byte[]> imgVerify(HttpServletRequest request) {
        // 生成验证码
        byte[] imgCache;
        HttpSession session = request.getSession();
        imgCache = captchaGenerator.captcha(session);
        final HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.IMAGE_PNG);
        return new ResponseEntity<>(imgCache, headers, HttpStatus.CREATED);
    }

    /**
     * 发送短信验证码
     *
     * @param mobile 手机号
     * @param request 请求
     * @return 短信发送结果
     */
    @RequestMapping(value = "/sms", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Boolean sendSms(@RequestParam("mobile") String mobile,
                           @RequestParam(value = "imgVerify", required = false) String imgVerify,
                           @RequestParam(value = "checkType", required = false) Integer type,
                           HttpServletRequest request) {
        if (mobilePattern.getPattern().matcher(mobile).matches()) {
            HttpSession session = request.getSession();
            String activateCode = (String) session.getAttribute("code");

            if (isMobileClient(request)) {
                log.debug("current is mobile client, do not need captcha");
            } else {
                // 判断图片验证码是否正确
                if (!Objects.equal(captchaGenerator.getGeneratedKey(session), imgVerify)) {
                    log.error("image verify error, image code not match");
                    throw new JsonResponseException(500, "image.code.error");
                }
            }

            //验证该手机号是否存在
            if(Objects.equal(type, 1)){
                User user = RespHelper.or500(userReadService.findBy(mobile, LoginType.MOBILE));
                if(Arguments.isNull(user)){
                    throw new JsonResponseException(500, "mobile.not.valid");
                }
            }

            Response<Boolean> result = null;
            if (!Strings.isNullOrEmpty(activateCode)) { //判断是否需要重新发送激活码
                List<String> parts = AT_SPLITTER.splitToList(activateCode);
                long sendTime = Long.parseLong(parts.get(1));
                if (System.currentTimeMillis() - sendTime < TimeUnit.MINUTES.toMillis(1)) { //
                    log.error("could not send sms, sms only can be sent once in one minute");
                    throw new JsonResponseException(500, "1分钟内只能获取一次验证码");
                } else {
                    String code = String.valueOf((int)((Math.random()*9+1)*100000));
                    session.setAttribute("code", code + "@" + System.currentTimeMillis()+"@"+mobile);
                    // 发送验证码
                    result = doSendSms(code, mobile);
                }
            } else { //新发送激活码
                String code = String.valueOf((int) ((Math.random() * 9 + 1) * 100000));
                session.setAttribute("code", code + "@" + System.currentTimeMillis()+"@"+mobile);
                // 发送验证码
                result = doSendSms(code, mobile);
            }
            if(!result.isSuccess()) {
                log.warn("send sms single fail, cause:{}", result.getError());
                throw new JsonResponseException(500, result.getError());
            }
            return result.getResult();
        } else {
            throw new JsonResponseException(400, "mobile.format.error");
        }
    }

    private boolean isMobileClient(HttpServletRequest request) {
        Integer isMobile = Strs.parseInt(request.getHeader("X-Mobile-Client")).or(0);
        return isMobile != 0;
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
            String result=smsWebService.send(mobile, "user.register.code",context, null);
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

    /**
     * 校验手机验证码
     *
     * @param code    输入的验证码
     * @param request 请求
     */
    private void validateSmsCode(String code, String mobile, HttpServletRequest request) {
        HttpSession session = request.getSession(true);
        // session verify, value = code@time@mobile
        String codeInSession = (String) session.getAttribute("code");
        checkArgument(notEmpty(codeInSession), "sms.token.error");
        String expectedCode = AT_SPLITTER.splitToList(codeInSession).get(0);
        checkArgument(equalWith(code, expectedCode), "sms.token.error");
        String expectedMobile = AT_SPLITTER.splitToList(codeInSession).get(2);
        checkArgument(equalWith(mobile, expectedMobile), "sms.token.error");

        // 如果验证成功则删除之前的code
        session.removeAttribute("code");
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
     * 修改手机号
     *
     * @param mobile  手机号
     * @param code    手机验证码
     * @param request
     * @return
     */
    @RequestMapping(value = "change_mobile", method = RequestMethod.POST)
    public String changeMobile(@RequestParam("mobile") String mobile,
                               @RequestParam("captcha") String code,
                               HttpServletRequest request){
        ParanaUser loginUser = UserUtil.getCurrentUser();
        Long userId = loginUser.getId();
        String tmp = (String)request.getSession().getAttribute("code");
        if (Strings.isNullOrEmpty(tmp)) {
            throw new JsonResponseException(500, "验证码不匹配");
        }

        List<String> value = Splitters.AT.splitToList(tmp);
        String excepted = value.get(0);
        if (!equalWith(excepted, code)) {
            throw new JsonResponseException(500, "验证码不匹配");
        }
        request.getSession().removeAttribute("code");

        User user = new User();
        user.setId(userId);
        user.setMobile(mobile);
        Response<Boolean> resp = userWriteService.update(user);
        if (!resp.isSuccess()) {
            log.error("fail to change mobile for user:{}, cause:{}", loginUser, resp.getError());
            throw new JsonResponseException(resp.getError());
        }
        return "ok";
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

    /**
     * 将用户ID放入Cookie
     *
     * @param request  请求
     * @param response 响应
     * @param userId   用户ID
     */
    private void setUserIdInCookie(HttpServletRequest request, HttpServletResponse response, Long userId) {
        WebUtil.addCookie(request, response, UN_ACTIVE_USER_ID, String.valueOf(userId), 1800);
    }

    /**
     * 给当前用户发送激活邮件
     *
     * @param request 请求
     * @return
     */
    @RequestMapping(value = "/email_active", method = RequestMethod.GET)
    @ResponseBody
    public Boolean emailActive(HttpServletRequest request){
        // TODO 邮箱注册
        return false;
    }

    @RequestMapping(value = "/change_password", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Boolean changePassword(String oldPassword, String newPassword) {
        Long userId = UserUtil.getUserId();
        if (userId == null) {
            throw new JsonResponseException(401, "user.not.login");
        }
        if (!newPassword.matches("[\\s\\S]{6,16}")) {
            log.warn("password syntax error");
            throw new JsonResponseException(500,"user.password.6to16");
        }
        log.debug("user {} want to change password at {}", userId, DateTime.now().toString("yyyy-MM-dd HH:mm:ss"));
        User user = RespHelper.or500(userReadService.findById(userId));
        if (!EncryptUtil.match(oldPassword, user.getPassword())) {
            log.warn("user old password{} isn't matched.", oldPassword);
            throw new JsonResponseException("user.old.password.not.match");
        }
        User toUpdate = new User();
        toUpdate.setId(userId);
        toUpdate.setPassword(EncryptUtil.encrypt(newPassword));
        Response<Boolean> result = userWriteService.update(toUpdate);
        if (result.isSuccess()) {
            return result.getResult();
        } else {
            log.warn("failed to change password for user id={},error code:{}", userId, result.getError());
            throw new JsonResponseException(500, result.getError());
        }
    }

    @RequestMapping(value = "/suggest", method = RequestMethod.GET)
    @ResponseBody
    public List<ParanaUser> suggestUser(@RequestParam String query) {
        LoginType type;
        if (CharMatcher.DIGIT.matchesAllOf(query)) {
            type = LoginType.MOBILE;
        } else if (CharMatcher.is('@').matchesAnyOf(query)) {
            type = LoginType.EMAIL;
        } else {
            type = LoginType.NAME;
        }
        Response<User> resp = userReadService.findBy(query, type);
        if (!resp.isSuccess() || resp.getResult() == null) {
            return Collections.emptyList();
        }
        ParanaUser u = GalaxyUserMaker.from(resp.getResult());
        return Lists.newArrayList(u);
    }
}

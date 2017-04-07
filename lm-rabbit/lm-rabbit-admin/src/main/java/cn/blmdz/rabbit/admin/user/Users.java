/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.admin.user;

import static cn.blmdz.home.common.util.Arguments.isNull;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.Objects;
import com.google.common.eventbus.EventBus;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.rabbit.common.enums.UserStatus;
import cn.blmdz.rabbit.user.util.GalaxyUserMaker;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.user.model.LoginType;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.AdminUserService;
import cn.blmdz.wolf.user.service.UserReadService;
import cn.blmdz.wolf.web.core.Constants;
import cn.blmdz.wolf.web.core.events.user.LoginEvent;
import cn.blmdz.wolf.web.core.events.user.LogoutEvent;
import lombok.extern.slf4j.Slf4j;

/**
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-01-30
 */
@Slf4j
@RestController
@RequestMapping("/api/user")
public class Users {

    private final UserReadService<User> userReadService;

    private final AdminUserService adminUserService;

    private final EventBus eventBus;

    @Autowired
    public Users(UserReadService<User> userReadService,
                 AdminUserService adminUserService, EventBus eventBus) {
        this.userReadService = userReadService;
        this.adminUserService = adminUserService;
        this.eventBus = eventBus;
    }

    @RequestMapping("")
    public BaseUser getLoginUser() {
        return UserUtil.getCurrentUser();
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
     * 批量更新用户状态
     * @param ids 用户id数组
     * @param status 用户状态
     * @return 更新成功返回true, 反之false
     */
    @RequestMapping(value = "/statuses", method = RequestMethod.POST)
    @ResponseBody
    public Boolean updateStatuses(@RequestParam(value = "ids[]") Long[] ids, @RequestParam(value = "status") Integer status){
        for (Long id : ids) {
            Response<Boolean> resp = adminUserService.updateStatus(id, status);
            if (!resp.isSuccess()){
                throw new JsonResponseException(resp.getError());
            }
        }
        return Boolean.TRUE;
    }
}

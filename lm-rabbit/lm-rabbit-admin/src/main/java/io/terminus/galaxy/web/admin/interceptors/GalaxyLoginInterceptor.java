/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package io.terminus.galaxy.web.admin.interceptors;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.util.GalaxyUserMaker;
import io.terminus.pampas.common.UserUtil;
import io.terminus.pampas.engine.common.WebUtil;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import io.terminus.parana.web.core.Constants;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.util.concurrent.TimeUnit;

import static io.terminus.galaxy.common.util.UserRoleUtil.isAdmin;
import static io.terminus.galaxy.common.util.UserRoleUtil.isOperator;

/**
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-01-28
 */
@Slf4j
@Component("loginInterceptor")
public class GalaxyLoginInterceptor extends HandlerInterceptorAdapter {

    private final LoadingCache<Long, Response<User>> userCache;

    private final UserReadService<User> userReadService;

    @Autowired
    public GalaxyLoginInterceptor(final UserReadService<User> userReadService) {
        this.userReadService = userReadService;
        userCache = CacheBuilder.newBuilder().expireAfterWrite(30, TimeUnit.MINUTES).build(new CacheLoader<Long, Response<User>>() {
            @Override
            public Response<User> load(Long userId) throws Exception {
                return userReadService.findById(userId);
            }
        });

    }

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
        WebUtil.putRequestAndResponse(request, response);

        HttpSession session = request.getSession(false);
        if (session != null) {
            Object userIdInSession = session.getAttribute(Constants.SESSION_USER_ID);
            if (userIdInSession != null) {

                final Long userId = Long.valueOf(userIdInSession.toString());
                Response<? extends User> result = userReadService.findById(userId);
                if (!result.isSuccess()) {
                    log.warn("failed to find user where id={},error code:{}", userId, result.getError());
                    return false;
                }
                User user = result.getResult();
                if (user != null) {
                    ParanaUser paranaUser = GalaxyUserMaker.from(user);
                    if(!isAdmin(paranaUser) && !isOperator(paranaUser)) {
                        log.warn("user(id={})'s is neither admin nor operator, its type is {}", userId, user.getType());
                    }
                    UserUtil.putCurrentUser(paranaUser);
                }
            }
        }
        return true;
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex) throws Exception {
        WebUtil.clear();
        UserUtil.clearCurrentUser();
    }
}

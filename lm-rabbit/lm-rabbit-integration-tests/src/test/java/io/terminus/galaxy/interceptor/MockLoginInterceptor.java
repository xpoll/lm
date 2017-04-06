package io.terminus.galaxy.interceptor;

import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.common.model.ParanaUser;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class MockLoginInterceptor extends HandlerInterceptorAdapter {

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
        ParanaUser paranaUser = new ParanaUser();
        paranaUser.setShopId(1L);
        paranaUser.setType(2);
        paranaUser.setId(1L);
        UserUtil.putCurrentUser(paranaUser);
        return true;
    }
}

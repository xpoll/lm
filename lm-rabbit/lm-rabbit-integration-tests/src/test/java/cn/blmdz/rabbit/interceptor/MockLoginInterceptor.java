package cn.blmdz.rabbit.interceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.wolf.common.model.ParanaUser;

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

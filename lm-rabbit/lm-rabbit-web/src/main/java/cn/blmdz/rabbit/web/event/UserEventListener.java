package cn.blmdz.rabbit.web.event;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.user.model.UserProfile;
import cn.blmdz.wolf.user.service.UserProfileWriteService;
import cn.blmdz.wolf.web.core.events.user.RegisterEvent;
import lombok.extern.slf4j.Slf4j;

/**
 * Date: 6/27/16
 * Time: 8:51 PM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
@Slf4j
@Component
@SuppressWarnings("unused")
public class UserEventListener {
    private final EventBus eventBus;
    private final UserProfileWriteService userProfileWriteService;

    @Autowired
    public UserEventListener(EventBus eventBus,
                             UserProfileWriteService userProfileWriteService) {
        this.eventBus = eventBus;
        this.userProfileWriteService = userProfileWriteService;
    }

    @PostConstruct
    public void init() {
        eventBus.register(this);
    }

    @Subscribe
    public void onUserRegister(RegisterEvent event) {
        final ParanaUser user = event.getUser();

        // 建立一个用户详情
        UserProfile userProfile = new UserProfile(user.getId());
        Response<Boolean> tryCreateProfile = userProfileWriteService.createProfile(userProfile);
        if (!tryCreateProfile.isSuccess()) {
            log.error("fail to create user profile for user:{}, cause:{}", user, tryCreateProfile.getError());
        }
    }
}

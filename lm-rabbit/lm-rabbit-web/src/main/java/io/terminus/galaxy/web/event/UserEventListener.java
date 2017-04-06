package io.terminus.galaxy.web.event;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import io.terminus.common.model.Response;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.user.model.UserProfile;
import io.terminus.parana.user.service.UserProfileWriteService;
import io.terminus.parana.web.core.events.user.RegisterEvent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

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

package io.terminus.galaxy.user.impl.service;

import io.terminus.parana.user.impl.dao.UserDao;
import io.terminus.parana.user.impl.service.UserWriteServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;

/**
 * @author Effet
 */
@Slf4j
@Component
@Primary
public class GalaxyUserWriteServiceImpl extends UserWriteServiceImpl {

    @Autowired
    public GalaxyUserWriteServiceImpl(UserDao userDao) {
        super(userDao);
    }
}

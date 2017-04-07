package cn.blmdz.rabbit.user.impl.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.user.impl.dao.UserDao;
import cn.blmdz.wolf.user.impl.service.UserWriteServiceImpl;
import lombok.extern.slf4j.Slf4j;

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

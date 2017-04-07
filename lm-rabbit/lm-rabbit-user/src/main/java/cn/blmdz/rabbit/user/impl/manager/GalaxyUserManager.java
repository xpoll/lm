package cn.blmdz.rabbit.user.impl.manager;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import cn.blmdz.rabbit.user.impl.dao.OperatorDao;
import cn.blmdz.rabbit.user.model.Operator;
import cn.blmdz.wolf.user.impl.dao.UserDao;
import cn.blmdz.wolf.user.model.User;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Effet
 */
@Slf4j
@Component
public class GalaxyUserManager {

    private final UserDao userDao;

    private final OperatorDao operatorDao;

    @Autowired
    public GalaxyUserManager(UserDao userDao, OperatorDao operatorDao) {
        this.userDao = userDao;
        this.operatorDao = operatorDao;
    }

    @Transactional
    public Long createOperator(User user, Operator operator) {
        Long userId = createUser(user);
        operator.setUserId(userId);
        operatorDao.create(operator);
        return userId;
    }

    private Long createUser(User user) {
        userDao.create(user);
        return user.getId();
    }
}

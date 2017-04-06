package io.terminus.galaxy.user.impl.manager;

import io.terminus.galaxy.user.impl.dao.OperatorDao;
import io.terminus.galaxy.user.model.Operator;
import io.terminus.parana.user.impl.dao.UserDao;
import io.terminus.parana.user.model.User;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

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

package cn.blmdz.rabbit.user.impl.dao;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.user.model.Operator;

/**
 * @author Effet
 */
@Repository
public class OperatorDao extends MyBatisDao<Operator> {

    public Operator findByUserId(Long userId) {
        return getSqlSession().selectOne(sqlId("findByUserId"), userId);
    }
}

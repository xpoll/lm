package io.terminus.galaxy.order.dao;


import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.order.model.UserVatInvoice;
import org.springframework.stereotype.Repository;

/**
 * Desc: 增值税开票信息DAO
 * Mail: hehaiyang@terminus.io
 * Date: 16/3/17
 */
@Repository
public class UserVatInvoiceDao extends MyBatisDao<UserVatInvoice> {

    public UserVatInvoice findByUserId(Long userId) {
        return getSqlSession().selectOne(sqlId("findByUserId"), userId);
    }
}

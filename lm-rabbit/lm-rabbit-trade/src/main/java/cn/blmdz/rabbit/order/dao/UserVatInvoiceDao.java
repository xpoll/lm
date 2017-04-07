package cn.blmdz.rabbit.order.dao;


import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.order.model.UserVatInvoice;

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

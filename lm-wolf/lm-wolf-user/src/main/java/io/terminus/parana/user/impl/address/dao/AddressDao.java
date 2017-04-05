package io.terminus.parana.user.impl.address.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.user.address.model.Address;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class AddressDao extends MyBatisDao {
   public Address findById(Integer id) {
      return (Address)this.getSqlSession().selectOne(this.sqlId("findById"), id);
   }

   public Address findByName(String name) {
      return (Address)this.getSqlSession().selectOne(this.sqlId("findByName"), name);
   }

   public List findByPid(Integer pid) {
      return this.getSqlSession().selectList(this.sqlId("findByPid"), pid);
   }

   public List findByLevel(Integer level) {
      return this.getSqlSession().selectList(this.sqlId("findByLevel"), level);
   }

   public Boolean createWithId(Address address) {
      return Boolean.valueOf(this.getSqlSession().insert(this.sqlId("createWithId"), address) == 1);
   }
}

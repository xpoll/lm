package cn.blmdz.wolf.user.dao;

import java.util.List;
import java.util.Map;

import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.wolf.user.model.SubDomain;
import cn.blmdz.wolf.user.model.SubDomainType;

@Repository
public class SubDomainDao {
   @Autowired
   private SqlSessionTemplate sqlSession;

   public Long create(SubDomain domain) {
      this.sqlSession.insert("SubDomain.create", domain);
      return domain.getId();
   }

   public Boolean update(SubDomain domain) {
      return Boolean.valueOf(this.sqlSession.update("SubDomain.update", domain) == 1);
   }

   public Boolean delete(Long id) {
      return Boolean.valueOf(this.sqlSession.delete("SubDomain.delete", id) == 1);
   }

   public SubDomain findById(Long id) {
      return (SubDomain)this.sqlSession.selectOne("SubDomain.findById", id);
   }

   public SubDomain findBy(SubDomain domain) {
      return (SubDomain)this.sqlSession.selectOne("SubDomain.findBy", domain);
   }

   public SubDomain findByTarget(Long targetId, SubDomainType type) {
      SubDomain domain = new SubDomain();
      domain.setTargetId(targetId);
      domain.setType(Integer.valueOf(type.value()));
      return this.findBy(domain);
   }

   public SubDomain findByDomain(String subDomain, SubDomainType type) {
      SubDomain domain = new SubDomain();
      domain.setType(Integer.valueOf(type.value()));
      domain.setValue(subDomain);
      return this.findBy(domain);
   }

   public List findByType(SubDomainType type) {
      SubDomain domain = new SubDomain();
      domain.setType(Integer.valueOf(type.value()));
      return this.sqlSession.selectList("SubDomain.findBy", domain);
   }

   public Paging paging(SubDomain criteria, Integer offset, Integer limit) {
      Map<String, Object> params = Maps.newHashMap();
      params.put("type", criteria.getType());
      params.put("targetId", criteria.getTargetId());
      params.put("value", criteria.getValue());
      Long count = (Long)this.sqlSession.selectOne("SubDomain.count", params);
      if(count.longValue() <= 0L) {
         return Paging.empty();
      } else {
         params.put("offset", offset);
         params.put("limit", limit);
         List<SubDomain> data = this.sqlSession.selectList("SubDomain.paging", params);
         return new Paging(count, data);
      }
   }
}

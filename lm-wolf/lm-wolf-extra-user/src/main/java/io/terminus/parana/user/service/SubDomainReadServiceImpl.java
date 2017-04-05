package io.terminus.parana.user.service;

import com.google.common.base.Optional;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import io.terminus.common.model.PageInfo;
import io.terminus.common.model.Response;
import io.terminus.parana.common.utils.RespHelper.Opt;
import io.terminus.parana.user.dao.SubDomainDao;
import io.terminus.parana.user.model.SubDomain;
import io.terminus.parana.user.model.SubDomainType;
import io.terminus.parana.user.service.SubDomainReadService;
import java.util.regex.Pattern;
import javax.annotation.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SubDomainReadServiceImpl implements SubDomainReadService {
   private static final Logger log = LoggerFactory.getLogger(SubDomainReadServiceImpl.class);
   private static final Pattern SUB_DOMAIN_PATTERN = Pattern.compile("^[A-Za-z0-9](?:[A-Za-z0-9\\-]{0,61}[A-Za-z0-9])?$");
   private final SubDomainDao subDomainDao;

   @Autowired
   public SubDomainReadServiceImpl(SubDomainDao subDomainDao) {
      this.subDomainDao = subDomainDao;
   }

   public Response get(long targetId, SubDomainType type) {
      return Opt.unwrap(this.getAllowNotFound(targetId, type), "sub.domain.no.exist");
   }

   public Response getAllowNotFound(long targetId, SubDomainType type) {
      try {
         if(type == null) {
            return Response.fail("sub.domain.type.invalid");
         } else {
            SubDomain domain = this.subDomainDao.findByTarget(Long.valueOf(targetId), type);
            return Response.ok(Optional.fromNullable(domain));
         }
      } catch (Exception var5) {
         log.error("sub domain query failed, targetId={}, type={}, cause:{}", new Object[]{Long.valueOf(targetId), type, Throwables.getStackTraceAsString(var5)});
         return Response.fail("sub.domain.find.fail");
      }
   }

   public Response getByDomain(String subDomain, SubDomainType type) {
      return Opt.unwrap(this.getByDomainAllowNotFound(subDomain, type), "sub.domain.no.exist");
   }

   public Response getByDomainAllowNotFound(String subDomain, SubDomainType type) {
      try {
         if(type == null) {
            return Response.fail("sub.domain.type.invalid");
         } else if(!Strings.isNullOrEmpty(subDomain) && SUB_DOMAIN_PATTERN.matcher(subDomain).matches()) {
            SubDomain domain = this.subDomainDao.findByDomain(subDomain, type);
            return Response.ok(Optional.fromNullable(domain));
         } else {
            log.warn("sub domain not valid, sub domain={}", subDomain);
            return Response.ok(Optional.absent());
         }
      } catch (Exception var4) {
         log.error("sub domain query failed, targetId={}, type={}, cause:{}", new Object[]{subDomain, type, Throwables.getStackTraceAsString(var4)});
         return Response.fail("sub.domain.find.fail");
      }
   }

   public Response getByType(SubDomainType type) {
      if(type == null) {
         return Response.fail("sub.domain.type.invalid");
      } else {
         try {
            return Response.ok(this.subDomainDao.findByType(type));
         } catch (Exception var3) {
            log.error("sub domain query failed, type={}, cause:{}", type, Throwables.getStackTraceAsString(var3));
            return Response.fail("sub.domain.find.fail");
         }
      }
   }

   public Response pagination(@Nullable Integer type, Integer pageNo, Integer size) {
      try {
         SubDomain criteria = new SubDomain();
         criteria.setType(type);
         PageInfo page = new PageInfo(pageNo, size);
         return Response.ok(this.subDomainDao.paging(criteria, page.getOffset(), page.getLimit()));
      } catch (Exception var6) {
         log.error("paging sub domains failed, type={}, pageNo={}, size={}, cause:{}", new Object[]{type, pageNo, size, Throwables.getStackTraceAsString(var6)});
         return Response.fail("sub.domain.find.fail");
      }
   }
}

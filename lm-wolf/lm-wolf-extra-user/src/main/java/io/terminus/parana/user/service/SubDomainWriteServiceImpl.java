package io.terminus.parana.user.service;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.parana.common.utils.ServiceUtils;
import io.terminus.parana.user.dao.SubDomainDao;
import io.terminus.parana.user.model.SubDomain;
import io.terminus.parana.user.model.SubDomainType;
import io.terminus.parana.user.service.SubDomainWriteService;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SubDomainWriteServiceImpl implements SubDomainWriteService {
   private static final Logger log = LoggerFactory.getLogger(SubDomainWriteServiceImpl.class);
   private static final Pattern SUB_DOMAIN_PATTERN = Pattern.compile("^[A-Za-z0-9](?:[A-Za-z0-9\\-]{0,61}[A-Za-z0-9])?$");
   private final SubDomainDao subDomainDao;

   @Autowired
   public SubDomainWriteServiceImpl(SubDomainDao subDomainDao) {
      this.subDomainDao = subDomainDao;
   }

   public Response setSubDomain(String value, int type, long targetId, String desc) {
      try {
         SubDomainType typeE = (SubDomainType)SubDomainType.parse(Integer.valueOf(type)).orNull();
         if(typeE == null) {
            log.warn("sub domain type={} not found", Integer.valueOf(type));
            return Response.fail("sub.domain.type.invalid");
         } else if(!Strings.isNullOrEmpty(value) && SUB_DOMAIN_PATTERN.matcher(value).matches()) {
            SubDomain sub = this.subDomainDao.findByTarget(Long.valueOf(targetId), typeE);
            if(sub != null) {
               if(!value.equalsIgnoreCase(sub.getValue())) {
                  this.checkExist(value);
               }

               sub.setValue(value);
               sub.setDesc(desc);
               this.subDomainDao.update(sub);
            } else {
               this.checkExist(value);
               sub = new SubDomain();
               sub.setValue(value);
               sub.setType(Integer.valueOf(type));
               sub.setTargetId(Long.valueOf(targetId));
               sub.setDesc(desc);
               this.subDomainDao.create(sub);
            }

            return Response.ok(sub.getId());
         } else {
            log.warn("sub domain not match, value={}", value);
            return Response.fail("sub.domain.value.invalid");
         }
      } catch (ServiceException var8) {
         return Response.fail(var8.getMessage());
      } catch (Exception var9) {
         log.error("set sub domain failed, value={}, type={}, targetId={}, cause:{}", new Object[]{value, Integer.valueOf(type), Long.valueOf(targetId), Throwables.getStackTraceAsString(var9)});
         return Response.fail("sub.domain.set.fail");
      }
   }

   public Response delete(long id) {
      try {
         return Response.ok(this.subDomainDao.delete(Long.valueOf(id)));
      } catch (Exception var4) {
         log.error("sub domain delete fail, id={}, cause:{}", Long.valueOf(id), Throwables.getStackTraceAsString(var4));
         return Response.fail("sub.domain.del.fail");
      }
   }

   private void checkExist(String value) throws ServiceException {
      ServiceUtils.checkResult(value != null, "sub.domain.value.invalid");
      SubDomain criteria = new SubDomain();
      criteria.setValue(value.toLowerCase());
      Paging<SubDomain> paging = this.subDomainDao.paging(criteria, Integer.valueOf(0), Integer.valueOf(0));
      ServiceUtils.checkResult(paging.getTotal().longValue() == 0L, "sub.domain.exist");
   }
}

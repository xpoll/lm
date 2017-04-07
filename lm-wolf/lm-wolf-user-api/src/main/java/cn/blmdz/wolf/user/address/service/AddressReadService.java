package cn.blmdz.wolf.user.address.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.user.address.model.Address;

public interface AddressReadService {
   @Export
   Response provinces();

   @Export(
      paramNames = {"provinceId"}
   )
   Response citiesOf(Integer var1);

   @Export(
      paramNames = {"cityId"}
   )
   Response regionsOf(Integer var1);

   @Export(
      paramNames = {"regionId"}
   )
   Response streetsOf(Integer var1);

   @Export(
      paramNames = {"id"}
   )
   Response<List<Address>> childAddressOf(Integer var1);

   @Export(
      paramNames = {"id"}
   )
   Response findById(Integer var1);

   @Export(
      paramNames = {"addressId"}
   )
   Response ancestorsOf(Integer var1);

   @Export(
      paramNames = {"addressId"}
   )
   Response ancestorOfAddresses(Integer var1);

   @Export(
      paramNames = {"pid"}
   )
   Response getTreeOf(Integer var1);

   Response findByName(String var1);
}
